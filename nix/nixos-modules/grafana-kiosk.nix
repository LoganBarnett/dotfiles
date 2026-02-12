################################################################################
# Displays a Grafana dashboard in a kiosk mode.
################################################################################
{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkEnableOption mkIf mkOption optionalAttrs;
  cfg = config.services.grafana-kiosk;
in {
  options = {
    services.grafana-kiosk = {
      enable = mkEnableOption
        "Use Grafana-kiosk to display a Grafana dashboard in kiosk mode.";
      url = mkOption {
        type = lib.types.str;
        description = ''
          The URL of the Grafana dashboard to display.
        '';
      };
      systemUser = mkOption {
        type = lib.types.str;
        default = "kiosk";
        description = ''
          The user to auto-login as.
        '';
      };
      systemGroup = mkOption {
        type = lib.types.str;
        default  = "kiosk";
        description = ''
          The group of the auto-login user.
        '';
      };
      package = mkOption {
        type = lib.types.package;
        default = pkgs.grafana-kiosk;
      };
    };
  };
  config = mkIf cfg.enable {
    age.secrets.grafana-kiosk-passphrase = {
      generator.script = "long-passphrase";
      group = cfg.systemGroup;
      mode = "0440";
      rekeyFile = ../secrets/grafana-kiosk-passphrase.age;
    };
    age.secrets.grafana-kiosk-passphrase-hashed = {
      generator = {
        script = "long-passphrase-hashed";
        dependencies = [
          config.age.secrets.grafana-kiosk-passphrase
        ];
      };
    };
    users.users.${cfg.systemUser} = {
      isNormalUser = true;
      group = cfg.systemGroup;
      extraGroups = [ "video" "input" ];
      hashedPasswordFile = config
        .age
        .secrets
        .grafana-kiosk-passphrase-hashed
        .path
      ;
    };
    users.groups.${cfg.systemGroup} = {};
    services.displayManager = {
      enable = true;
      autoLogin = {
        enable = true;
        user = cfg.systemUser;
      };
    };
    # Hide the mouse cursor.
    services.unclutter-xfixes.enable = true;
    services.xserver = {
      enable = true;
      # Optional: Disable screen blanking.
      serverFlagsSection = ''
        Option "BlankTime" "0"
        Option "StandbyTime" "0"
        Option "SuspendTime" "0"
        Option "OffTime" "0"
      '';
      displayManager = {
        # lxde.enable = true;
        lightdm.enable = true;
        # Optional: Turn off the screen saver and DPMS.
        sessionCommands = ''
          xset s off
          xset -dpms
          xset s noblank
        '';
        session = [
          {
            name = "kiosk";
            manage = "windowManager";
            start = ''
            ${pkgs.openbox}/bin/openbox-session &
            systemctl --user start kiosk.service
            waitPID=$!
          '';
          }
        ];
      };
      windowManager.openbox.enable = true;
    };
    systemd.user.services.grafana-kiosk = {
      enable = true;
      description = "Grafana Kiosk";
      after = [ "network.target" ];
      wantedBy = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = let
        in ''
          ${cfg.package}/bin/grafana-kiosk -URL "${cfg.url}"
        '';
        Restart = "always";
        Environment = [
          "XDG_RUNTIME_DIR=%t"
          # Disable GPU to reduce memory usage.
          "KIOSK_GPU_ENABLED=false"
        ];
      };
    };
  };
}
