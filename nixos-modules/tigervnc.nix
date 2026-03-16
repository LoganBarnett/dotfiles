################################################################################
# Add a VNC server using TigerVNC.
################################################################################
{ config, lib, pkgs, ... }:

let
  cfg = config.services.tigervnc;
  inherit (lib) types;
  inherit (lib) mkOption;
in
{
  options.services.tigervnc = {
    package = mkOption {
      type = types.package;
      default = pkgs.tigervnc;
      description = "TigerVNC package to use.";
    };

    portBase = mkOption {
      type = types.int;
      default = 5900;
      description = "Base port for VNC; each display increments from here.";
    };

    displays = mkOption {
      type = types.attrsOf (types.submodule {
        options = {
          user = mkOption {
            type = types.str;
            description = "User account to run the VNC server under.";
          };
          geometry = mkOption {
            type = types.str;
            default = "1920x1080";
            description = "Screen geometry.";
          };
          depth = mkOption {
            type = types.int;
            default = 24;
            description = "Color depth.";
          };
          passwordFile = mkOption {
            type = types.path;
            description = "Path to the VNC password file.";
          };
          openFirewall = mkOption {
            type = types.bool;
            default = false;
            description = "Whether to open the firewall for this display.";
          };
          xstartup = mkOption {
            type = types.str;
            description = "Path of script to start the X session with.";
          };
        };
      });
      default = {};
      description = ''
        Set of displays to start, keyed by display number (e.g., \"1\").
      '';
    };
  };

  config = {
    environment.systemPackages = [ cfg.package ];

    systemd.services = lib.mkMerge (
      lib.mapAttrsToList (displayNum: displayCfg:
        let
          port = cfg.portBase + builtins.parseInt displayNum;
        in {
          "tigervnc-${displayNum}" = {
            description = "TigerVNC Server on :${displayNum}";
            after = [ "network.target" "run-agenix.d.mount" ];
            wants = [ "run-agenix.d.mount" ];
            wantedBy = [ "multi-user.target" ];
            path = [ pkgs.xorg.xinit ];
            serviceConfig = let
              kill-command = "${pkgs.coreutils}/bin/kill -TERM $MAINPID";

            in {
              Type = "simple";
              Environment = [
                "XDG_SESSION_TYPE=x11"
                "XDG_CURRENT_DESKTOP=GNOME"
              ];
              ExecStart = ''
                ${cfg.package}/bin/Xvnc :${displayNum} \
                  -geometry ${displayCfg.geometry} \
                  -depth ${toString displayCfg.depth} \
                  -SecurityTypes=None \
                  -PasswordFile=${lib.escapeShellArg displayCfg.passwordFile}
              '';
              # The service doesn't support reloading but without this a reload
              # (which NixOS does on nixos-rebuild switch) will end in failure.
              # ExecReload = "${cfg.package}/bin/vncserver ${toString displayNum} -kill";
              # ExecStop = "${cfg.package}/bin/vncserver ${toString displayNum} -kill";
              ExecReload = kill-command;
              ExecStop = kill-command;
                # ${cfg.package}/bin/vncserver :${displayNum}
                  # ${lib.escapeShellArg displayCfg.passwordFile}
              # ExecStop = "${cfg.package}/bin/vncserver -kill :${displayNum}";
              Restart = "always";
              User = displayCfg.user;
            };
            reloadIfChanged = true;
            restartTriggers = [ "on-failure" ];
          };
        }
      ) cfg.displays
    );

    networking.firewall.allowedTCPPorts = lib.flatten (
      lib.mapAttrsToList (displayNum: displayCfg:
        if displayCfg.openFirewall then
          [ (cfg.portBase + lib.strings.toInt displayNum) ]
        else
          []
      ) cfg.displays
    );
  };
}
