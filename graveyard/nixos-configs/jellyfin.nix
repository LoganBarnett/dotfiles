################################################################################
# TOMBSTONE — Jellyfin is no longer hosted.
#
# Jellyfin is a free and open-source media server.  It served the contents of
# /tank/data/media, which Metube populates via yt-dlp downloads.
#
# Jellyfin requires manual administration via its web UI to become operable
# after a fresh installation (initial user creation, library setup, etc.),
# which is incompatible with a fully declarative NixOS deployment.
#
# This file is not imported by any host and is kept for historical reference.
################################################################################
{ config, facts, ... }:
let
  port = 8096;
in
{
  services.jellyfin = {
    enable = false;
    openFirewall = false;
  };

  services.https.fqdns."jellyfin.${facts.network.domain}" = {
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
