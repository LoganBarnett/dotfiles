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
        description = ''
          The Kodi package to use.
        '';
      };
      advancedSettings = mkOption {
        type = lib.types.attrsOf (lib.types.attrsOf lib.types.str);
        default = {};
        description = ''
          Advanced Kodi settings to configure via advancedsettings.xml.
          These settings override guisettings.xml and are hidden from the UI.

          Settings are specified as nested attrsets matching the XML structure.
          See https://kodi.wiki/view/Advancedsettings.xml for available settings.
        '';
        example = {
          services = {
            webserver = "true";
            esallinterfaces = "true";
          };
          addons = {
            unknownsources = "true";
          };
        };
      };
      addonSettings = mkOption {
        type = lib.types.attrsOf (lib.types.attrsOf lib.types.str);
        default = {};
        description = ''
          Addon-specific settings to configure via userdata/addon_data/*/settings.xml.
          These files use the <settings version="2"> format with <setting id="key">value</setting>.

          Settings are specified as addon-path -> setting-id -> value.
        '';
        example = {
          "plugin.video.jellyfin" = {
            ipaddress = "localhost";
            port = "8096";
            https = "false";
          };
          "inputstream.adaptive" = {
            DECRYPTERPATH = "special://home/cdm";
          };
        };
      };
      enabledAddons = mkOption {
        type = lib.types.listOf lib.types.str;
        default = [];
        description = ''
          List of addon IDs to enable automatically on Kodi startup via JSON-RPC.
        '';
        example = [
          "plugin.video.jellyfin"
          "inputstream.adaptive"
          "inputstream.ffmpegdirect"
        ];
      };
      mediaSources = mkOption {
        type = lib.types.attrsOf (lib.types.listOf (lib.types.submodule {
          options = {
            name = mkOption {
              type = lib.types.str;
              description = "Display name for this media source.";
            };
            path = mkOption {
              type = lib.types.str;
              description = "File path or network location for this media source.";
            };
          };
        }));
        default = {};
        description = ''
          Declarative media sources to configure via sources.xml.
          Sources are organized by media type (video, music, pictures, programs).

          This allows fully declarative configuration of Kodi's media library
          without requiring interactive setup through the UI.
        '';
        example = {
          video = [
            { name = "Movies"; path = "/mnt/kodi-media/Movies/"; }
            { name = "TV Shows"; path = "/mnt/kodi-media/TV/"; }
          ];
          music = [
            { name = "Music Library"; path = "/mnt/kodi-media/Music/"; }
          ];
        };
      };
    };
  };
  config = mkIf cfg.enable (lib.mkMerge [
    {
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
    environment.systemPackages = [
      # Can be useful for querying some of Kodi's databases for debugging
      # purposes.
      pkgs.sqlite
    ];
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
  }
  (mkIf (cfg.enabledAddons != []) {
    # Service to enable addons via JSON-RPC after Kodi starts.
    systemd.user.services.kodi-enable-addons = {
      description = "Enable Kodi addons via JSON-RPC";
      after = [ "kodi.service" ];
      wantedBy = [ "default.target" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = false;
        ExecStart = pkgs.writeShellScript "kodi-enable-addons" ''
          # Wait for Kodi's JSON-RPC web server to be available.
          for i in {1..30}; do
            if ${pkgs.curl}/bin/curl \
                 --silent \
                 http://localhost:8080/jsonrpc &>/dev/null;
            then
              break
            fi
            sleep 1
          done

          # Enable each addon in the list.
          ${builtins.concatStringsSep "\n" (
            map (addonId: ''
              ${pkgs.curl}/bin/curl \
                --silent \
                --header "Content-Type: application/json" \
                --data '{
                  "jsonrpc":"2.0",
                  "method":"Addons.SetAddonEnabled",
                  "params":{"addonid":"${addonId}","enabled":true},
                  "id":1
                }' \
                http://localhost:8080/jsonrpc
            '') cfg.enabledAddons
          )}
        '';
      };
    };
  })
  (mkIf (cfg.advancedSettings != {}) {
    # Generate advancedsettings.xml from the advancedSettings option.
    home-manager.users.${cfg.systemUser} = {
      home.file.".kodi/userdata/advancedsettings.xml".text = let
        advancedSettingsXml = ''
          <advancedsettings>
            ${builtins.concatStringsSep "\n" (
              lib.mapAttrsToList (section: settings:
                ''<${section}>
              ${builtins.concatStringsSep "\n    " (
                lib.mapAttrsToList (name: value:
                  ''<${name}>${value}</${name}>''
                ) settings
              )}
            </${section}>''
              ) cfg.advancedSettings
            )}
          </advancedsettings>
        '';
      in advancedSettingsXml;
    };
  })
  (mkIf (cfg.addonSettings != {}) {
    # Generate addon settings XML files from the addonSettings option.
    home-manager.users.${cfg.systemUser} = {
      home.file = lib.mapAttrs' (addonPath: settings:
        lib.nameValuePair ".kodi/userdata/addon_data/${addonPath}/settings.xml" {
          text = ''
            <settings version="2">
              ${builtins.concatStringsSep "\n  " (
                lib.mapAttrsToList (settingId: value:
                  ''<setting id="${settingId}">${value}</setting>''
                ) settings
              )}
            </settings>
          '';
        }
      ) cfg.addonSettings;
    };
  })
  (mkIf (cfg.mediaSources != {}) {
    # Generate sources.xml from the mediaSources option.
    home-manager.users.${cfg.systemUser} = {
      home.file.".kodi/userdata/sources.xml".text = let
        sourcesXml = ''
          <sources>
            ${builtins.concatStringsSep "\n  " (
              lib.mapAttrsToList (mediaType: sources:
                ''<${mediaType}>
              ${builtins.concatStringsSep "\n    " (
                map (source:
                  ''<source>
                  <name>${source.name}</name>
                  <path pathversion="1">${source.path}</path>
                </source>''
                ) sources
              )}
            </${mediaType}>''
              ) cfg.mediaSources
            )}
          </sources>
        '';
      in sourcesXml;
    };
  })
]);
}
