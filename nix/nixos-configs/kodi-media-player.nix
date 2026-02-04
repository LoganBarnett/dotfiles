################################################################################
# Kodi standalone mode for TV media playback.
#
# This configuration runs Kodi in fullscreen mode, reading media files directly
# from NFS-mounted storage. The system auto-boots to Kodi and keeps the TV as
# a dumb display. Kodi manages its own media library declaratively without
# needing a separate media server.
################################################################################
{ config, host-id, lib, pkgs, ... }: {
  imports = [
    ../nixos-modules/kodi-standalone.nix
    ../nixos-modules/nfs-consumer-facts.nix
    ../nixos-modules/nfs-mount-consumer.nix
  ];

  # Configure NFS mount for media files.
  nfsConsumerFacts = {
    enable = true;
    provider = {
      remoteHost = "silicon.proton";
      vpnHost = "silicon-nas.proton";
      providerHostId = "silicon";
      wgPort = 51821;
    };
  };
  services.kodi-standalone = {
    enable = true;
    package = pkgs.kodi.withPackages (kodiPkgs: with kodiPkgs; [
      # jellyfin addon removed - reading media files directly from NFS
      inputstream-adaptive
      inputstream-ffmpegdirect
    ]);
    advancedSettings = {
      services = {
        # Enable web server for remote control via mobile apps.
        webserver = "true";

        # Disable authentication for JSON-RPC access.
        webserverauthentication = "false";

        # Allow remote control from applications on other systems (needed for iOS app).
        esallinterfaces = "true";
      };
      addons = {
        # Allow installation of addon dependencies without prompts.
        unknownsources = "true";
      };
      audio = {
        # Boost audio volume by 12dB. Many DVDs are encoded with low audio levels,
        # and this amplification compensates without distortion. Adjust if needed.
        volumeamplification = "12.0";
      };
    };
    addonSettings = {
      "plugin.video.jellyfin" = {
        ipaddress = "localhost";
        port = "8096";
        https = "false";
        sslverify = "true";
      };
      "inputstream.ffmpegdirect" = {
        streamselection = "0";
      };
      "inputstream.adaptive" = {
        DECRYPTERPATH = "special://home/cdm";
      };
    };
    enabledAddons = [
      "plugin.video.jellyfin"
      "inputstream.adaptive"
      "inputstream.ffmpegdirect"
    ];
    mediaSources = {
      video = [
        {
          name = "Media Library";
          path = "/mnt/kodi-media/";
        }
        {
          name = "NextCloud Uploads";
          path = "/mnt/nextcloud-shared-media/";
        }
      ];
    };
  };

  # Expose Kodi's web interface for remote control via mobile apps.
  # kodi-lv = Kodi in the living room.
  services.https.fqdns."kodi-living-room.proton" = {
    enable = true;
    internalPort = 8080;
  };

  # Open port 8080 for direct HTTP access. Required for iOS app remote control.
  # The Kodi iOS app does not support HTTPS endpoints and requires direct HTTP
  # access on port 8080. Android HTTPS support is not known.
  networking.firewall.allowedTCPPorts = [ 8080 ];

  # Add kodi user to media-shared group (created by nfs-consumer-facts).
  users.users.${config.services.kodi-standalone.systemUser}.extraGroups = [ "media-shared" ];

  # Pre-configure Kodi for the kodi user.
  home-manager.users.${config.services.kodi-standalone.systemUser} = {
    home.stateVersion = "25.11";

    # Configure WirePlumber to prefer HDMI audio output over analog.
    # This ensures audio routes to the TV via HDMI instead of the Mac Mini's
    # built-in speakers.
    xdg.configFile."wireplumber/main.lua.d/51-hdmi-default.lua".text = ''
      -- Set HDMI audio as the default sink.
      -- Matches HDA Intel HDMI devices and increases their priority.
      rule = {
        matches = {
          {
            { "node.name", "matches", "alsa_output.pci-0000_00_03.0.hdmi-*" },
          },
        },
        apply_properties = {
          ["priority.session"] = 2000,  -- Higher than default 1009
          ["node.default"] = true,
        },
      }

      table.insert(alsa_monitor.rules, rule)
    '';

    # Set HDMI audio sink volume to 100% on startup.
    systemd.user.services.set-hdmi-volume = {
      Unit = {
        Description = "Set HDMI audio volume to 100%";
        After = [ "pipewire.service" ];
      };
      Service = {
        Type = "oneshot";
        ExecStart = "${pkgs.pulseaudioFull}/bin/pactl set-sink-volume alsa_output.pci-0000_00_03.0.hdmi-stereo 100%";
        RemainAfterExit = false;
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
    };
  };
}
