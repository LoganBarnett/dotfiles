################################################################################
# Persistent terminal session management for remote development.
#
# This module provides lightweight session persistence using dtach, which is
# ideal for use with Emacs eat terminal emulator.  Unlike screen or tmux, dtach
# doesn't intercept keybindings, making it perfect for preserving eat's
# ergonomic evil-mode bindings.
#
# Sessions persist across network disconnections and can be resumed from any
# SSH connection.
################################################################################
{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.persistentTerminal;
in {
  options.services.persistentTerminal = {
    enable = mkEnableOption "persistent terminal sessions with dtach";

    sessionDirectory = mkOption {
      type = types.path;
      default = "/var/lib/persistent-sessions";
      description = ''
        Directory where dtach socket files are stored.  Each session creates
        a socket file in this directory.
      '';
    };

    users = mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        List of users allowed to create persistent sessions.
      '';
    };

    shellWrapper = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether to install a shell wrapper script that makes it easy to
        create and attach to sessions.
      '';
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      dtach
      (mkIf cfg.shellWrapper (pkgs.writeScriptBin "pts" ''
        #!${pkgs.bash}/bin/bash
        # pts - Persistent Terminal Session manager
        # Wrapper around dtach for easy session management.

        SOCKET_DIR="${cfg.sessionDirectory}/$USER"
        mkdir -p "$SOCKET_DIR"

        usage() {
          echo "Usage: pts [COMMAND] [SESSION_NAME]"
          echo ""
          echo "Commands:"
          echo "  new NAME    Create a new session"
          echo "  attach NAME Attach to an existing session"
          echo "  list        List all sessions"
          echo "  help        Show this help"
          echo ""
          echo "If no command is given, 'attach' is assumed."
        }

        list_sessions() {
          echo "Active sessions:"
          for socket in "$SOCKET_DIR"/*.socket 2>/dev/null; do
            if [[ -e "$socket" ]]; then
              basename "$socket" .socket
            fi
          done
        }

        case "$1" in
          new)
            if [[ -z "$2" ]]; then
              echo "Error: Session name required"
              usage
              exit 1
            fi
            echo "Creating new session: $2"
            exec ${pkgs.dtach}/bin/dtach -c "$SOCKET_DIR/$2.socket" -z $SHELL
            ;;
          attach|"")
            SESSION_NAME="''${2:-default}"
            SOCKET_PATH="$SOCKET_DIR/$SESSION_NAME.socket"
            if [[ ! -e "$SOCKET_PATH" ]]; then
              echo "Session '$SESSION_NAME' not found. Creating new session..."
              exec ${pkgs.dtach}/bin/dtach -c "$SOCKET_PATH" -z $SHELL
            else
              echo "Attaching to session: $SESSION_NAME"
              exec ${pkgs.dtach}/bin/dtach -a "$SOCKET_PATH" -z
            fi
            ;;
          list)
            list_sessions
            ;;
          help)
            usage
            ;;
          *)
            echo "Unknown command: $1"
            usage
            exit 1
            ;;
        esac
      ''))
    ];

    # Create the session directory with proper permissions.
    systemd.tmpfiles.rules = [
      "d ${cfg.sessionDirectory} 0755 root root -"
    ] ++ (map (user:
      "d ${cfg.sessionDirectory}/${user} 0700 ${user} users -"
    ) cfg.users);

    # Optional: SSH configuration to make reconnection seamless.
    programs.ssh.extraConfig = ''
      # Enable connection multiplexing for faster reconnects.
      Host *
        ControlMaster auto
        ControlPath ~/.ssh/control-%h-%p-%r
        ControlPersist 10m
    '';

    # Create a systemd user service template for managing sessions.
    systemd.user.services."persistent-session@" = {
      description = "Persistent terminal session %i";
      serviceConfig = {
        Type = "forking";
        ExecStart = "${pkgs.dtach}/bin/dtach -c %t/pts-%i.socket -z ${pkgs.bash}/bin/bash";
        Restart = "on-failure";
        RestartSec = 5;
      };
    };
  };
}