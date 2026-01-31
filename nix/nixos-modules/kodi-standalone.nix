################################################################################
# Runs Kodi in standalone mode for TV playback.
################################################################################
{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkEnableOption mkIf mkOption;
  cfg = config.services.kodi-standalone;
in {
  options = {
    services.kodi-standalone = {
      enable = mkEnableOption
        "Run Kodi in standalone mode as a media center interface.";
      systemUser = mkOption {
        type = lib.types.str;
        default = "kodi";
        description = ''
          The user to auto-login as.
        '';
      };
      systemGroup = mkOption {
        type = lib.types.str;
        default = "kodi";
        description = ''
          The group of the auto-login user.
        '';
      };
      package = mkOption {
        type = lib.types.package;
        default = pkgs.kodi.withPackages (kodiPkgs: with kodiPkgs; [
          jellyfin
          inputstream-adaptive
          inputstream-ffmpegdirect
        ]);
        description = ''
          The Kodi package to use, with Jellyfin addon by default.
        '';
      };
    };
  };
  config = mkIf cfg.enable {
    age.secrets.kodi-passphrase = {
      generator.script = "long-passphrase";
      group = cfg.systemGroup;
      mode = "0440";
      rekeyFile = ../secrets/kodi-passphrase.age;
    };
    age.secrets.kodi-passphrase-hashed = {
      generator = {
        script = "long-passphrase-hashed";
        dependencies = [
          config.age.secrets.kodi-passphrase
        ];
      };
    };
    users.users.${cfg.systemUser} = {
      isNormalUser = true;
      group = cfg.systemGroup;
      extraGroups = [ "video" "audio" "input" "render" ];
      hashedPasswordFile = config
        .age
        .secrets
        .kodi-passphrase-hashed
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
      # Disable screen blanking - we want continuous playback.
      serverFlagsSection = ''
        Option "BlankTime" "0"
        Option "StandbyTime" "0"
        Option "SuspendTime" "0"
        Option "OffTime" "0"
      '';
      displayManager = {
        lightdm.enable = true;
        # Turn off screen saver and power management.
        sessionCommands = ''
          xset s off
          xset -dpms
          xset s noblank
        '';
        session = [
          {
            name = "kodi";
            manage = "windowManager";
            start = ''
            ${pkgs.openbox}/bin/openbox-session &
            systemctl --user start kodi.service
            waitPID=$!
          '';
          }
        ];
      };
      windowManager.openbox.enable = true;
    };
    systemd.user.services.kodi = {
      enable = true;
      description = "Kodi Media Center";
      after = [ "graphical-session.target" ];
      wantedBy = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = ''
          ${cfg.package}/bin/kodi --standalone
        '';
        Restart = "always";
        Environment = "XDG_RUNTIME_DIR=%t";
      };
    };
    # Enable hardware acceleration for video playback.
    hardware.graphics.enable = true;
  };
}
