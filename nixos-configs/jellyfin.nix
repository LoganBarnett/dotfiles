################################################################################
# Jellyfin is a free and open-source media server.  It serves the contents of
# /tank/data/media, which Metube populates via yt-dlp downloads.
#
# NOTE: First boot requires a one-time interactive setup via the web UI at
# https://jellyfin.proton.  When prompted for a media library, point it at
# /tank/data/media.
################################################################################
{ config, ... }:
let
  port = 8096;
in
{
  services.jellyfin = {
    enable = false;
    openFirewall = false;
  };

  services.https.fqdns."jellyfin.proton" = {
    enable = false;
    internalPort = port;
  };

  # Start only after the data disk containing the media directory is mounted.
  # systemd.services.jellyfin = {
  #   after = [ "tank-data.mount" "setup-media-dir.service" ];
  #   requires = [ "tank-data.mount" ];
  # };

  # Grant Jellyfin read access to the shared media directory and allow
  # hardware-accelerated transcoding via the DRM render node.
  # users.users.jellyfin.extraGroups = [
  #   "media-shared"
  #   "render"
  #   "video"
  # ];

  # hardware.graphics.enable = true;
}
