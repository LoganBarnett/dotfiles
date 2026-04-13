################################################################################
# Terminal kiosk mode for dedicated monitoring displays.
#
# Runs a locked-down TTY that auto-logs in a kiosk user whose shell is a
# wrapper script.  The wrapper launches a configured program (e.g. btop),
# and when it exits shows a countdown with TTY-switch instructions before
# restarting.  No usable shell is ever exposed.
################################################################################
{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    ;
  cfg = config.services.terminal-kiosk;
  programCount = builtins.length cfg.programs;
  runtimeDeps = [
    pkgs.ncurses # tput / clear
  ]
  ++ lib.optionals (programCount > 1) [
    pkgs.dialog
  ];
  wrapperScript = pkgs.writeShellApplication {
    name = "terminal-kiosk-shell";
    runtimeInputs = runtimeDeps;
    text = ''
      # Disable job-control signals so the user cannot escape to a shell.
      trap "" TSTP QUIT

      while true; do
        CHOSEN_CMD=""
        ${
          if programCount == 1 then
            ''
              CHOSEN_CMD=${lib.escapeShellArg (builtins.head cfg.programs).command}
            ''
          else
            ''
              # Build a dialog menu from the configured programs.
              CHOICE=$(dialog --clear --title "Terminal Kiosk" \
                --menu "Select a program:" 20 60 ${toString programCount} \
                ${
                  lib.concatMapStringsSep " " (
                    p: "${lib.escapeShellArg p.name} ${lib.escapeShellArg p.command}"
                  ) cfg.programs
                } \
                2>&1 >/dev/tty) || true
              if [ -z "$CHOICE" ]; then
                continue
              fi
              # dialog returns the tag (name); look up the matching command.
              ${lib.concatMapStringsSep "\n" (p: ''
                if [ "$CHOICE" = ${lib.escapeShellArg p.name} ]; then
                  CHOSEN_CMD=${lib.escapeShellArg p.command}
                fi
              '') cfg.programs}
            ''
        }
        if [ -z "$CHOSEN_CMD" ]; then
          continue
        fi

        # Let the program handle Ctrl+C normally.
        trap - INT
        eval "$CHOSEN_CMD" || true
        # Re-lock Ctrl+C for the countdown screen.
        trap "" INT

        # Countdown screen — Ctrl+C restarts the loop via trap.
        RESTART=0
        trap "RESTART=1" INT
        clear
        for i in $(seq ${toString cfg.idleTimeout} -1 1); do
          tput cup 2 0
          echo "Program exited.  Restarting in $i seconds..."
          echo ""
          echo "To login, switch to another TTY:  Ctrl+Alt+F2"
          if [ "$RESTART" -eq 1 ]; then
            break
          fi
          sleep 1
        done
        trap "" INT
      done
    '';
  };
in
{
  options = {
    services.terminal-kiosk = {
      enable = mkEnableOption "Locked-down terminal kiosk on a dedicated TTY.";
      tty = mkOption {
        type = lib.types.int;
        default = 1;
        description = ''
          The TTY number to run the kiosk on.
        '';
      };
      user = mkOption {
        type = lib.types.str;
        default = "kiosk";
        description = ''
          The system user that runs the kiosk session.
        '';
      };
      group = mkOption {
        type = lib.types.str;
        default = "kiosk";
        description = ''
          The primary group of the kiosk user.
        '';
      };
      programs = mkOption {
        type = lib.types.listOf (
          lib.types.submodule {
            options = {
              name = mkOption {
                type = lib.types.str;
                description = "Display name for the program.";
              };
              command = mkOption {
                type = lib.types.str;
                description = "Command to run (full store path).";
              };
            };
          }
        );
        default = [ ];
        description = ''
          Programs available in the kiosk.  If only one is configured it
          launches directly; multiple programs show a selection menu.
        '';
      };
      idleTimeout = mkOption {
        type = lib.types.int;
        default = 60;
        description = ''
          Seconds to show the "program exited" countdown before restarting.
        '';
      };
    };
  };
  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.programs != [ ];
        message = "services.terminal-kiosk.programs must contain at least one program.";
      }
    ];

    # Create the kiosk user with the wrapper script as its login shell.
    users.users.${cfg.user} = {
      isNormalUser = true;
      group = cfg.group;
      # Locked password — no interactive or SSH login possible.
      initialHashedPassword = "!";
      extraGroups = [ "video" ];
      shell = "${wrapperScript}/bin/terminal-kiosk-shell";
    };
    users.groups.${cfg.group} = { };

    # Register the wrapper as a valid login shell.
    environment.shells = [ "${wrapperScript}/bin/terminal-kiosk-shell" ];

    # Autologin the kiosk user on the designated TTY.
    systemd.services."getty@tty${toString cfg.tty}" = {
      overrideStrategy = "asDropin";
      serviceConfig = {
        ExecStart = [
          # Clear the inherited ExecStart before setting a new one.
          ""
          "${pkgs.util-linux}/bin/agetty --autologin ${cfg.user} --noclear %I $TERM"
        ];
        Restart = "always";
      };
    };

    # Prevent the kernel from blanking the TTY.
    boot.kernelParams = [ "consoleblank=0" ];
  };
}
