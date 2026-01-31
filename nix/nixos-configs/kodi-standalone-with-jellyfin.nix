################################################################################
# Kodi standalone mode for TV media playback.
#
# This configuration runs Kodi in fullscreen mode with the Jellyfin addon,
# connecting to the local Jellyfin server. The system auto-boots to Kodi and
# keeps the TV as a dumb display.
################################################################################
{ ... }: {
  imports = [
    ../nixos-modules/kodi-standalone.nix
  ];
  services.kodi-standalone = {
    enable = true;
  };
}
