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

  # Pre-configure Kodi for the kodi user.
  home-manager.users.${config.services.kodi-standalone.systemUser} = {
    home.stateVersion = "25.11";
  };
}
