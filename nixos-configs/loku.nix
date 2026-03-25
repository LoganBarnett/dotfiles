################################################################################
# Loku is a local video browser and player.  It serves the contents of
# /tank/data/media (the same directory Metube populates via yt-dlp downloads),
# providing a web UI for browsing and playing video files without a database.
################################################################################
{ facts, ... }:
{
  services.loku-web = {
    enable = true;
    libraryPath = "/tank/data/media";
  };

  services.https.fqdns."loku.${facts.network.domain}" = {
    enable = true;
    # loku-web uses systemd socket activation; the socket lives at
    # /run/loku-web/loku-web.sock, which matches the serviceNameForSocket
    # convention.  https.nix adds nginx to the loku-web group so it can
    # connect to the group-readable socket.
    serviceNameForSocket = "loku-web";
  };

  # Ensure the media directory is mounted and initialised before the service
  # starts.  setup-media-dir.service creates /tank/data/media and sets the
  # media-shared ACLs.
  systemd.services.loku-web = {
    after = [
      "tank-data.mount"
      "setup-media-dir.service"
    ];
    requires = [ "tank-data.mount" ];
  };

  # Grant loku-web read access to files created under the media-shared group
  # ACL.  Without this the service user cannot read downloads that were written
  # by other members of media-shared (e.g. metube).
  users.users.loku-web.extraGroups = [ "media-shared" ];
}
