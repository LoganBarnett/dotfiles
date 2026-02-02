################################################################################
# Kodi standalone mode for TV media playback.
#
# This configuration runs Kodi in fullscreen mode with the Jellyfin addon,
# connecting to the local Jellyfin server. The system auto-boots to Kodi and
# keeps the TV as a dumb display.
################################################################################
{ config, pkgs, ... }: {
  imports = [
    ../nixos-modules/kodi-standalone.nix
  ];
  services.kodi-standalone = {
    enable = true;
    package = pkgs.kodi.withPackages (kodiPkgs: with kodiPkgs; [
      jellyfin
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
