################################################################################
# Metube is a web frontend for yt-dlp.  Paste a URL in the UI and the media
# lands in /tank/data/media where Jellyfin can pick it up.
################################################################################
{ ... }:
{
  imports = [ ../nixos-modules/metube.nix ];
  networking.dnsAliases = [ "metube" ];

  services.metube-host = {
    enable = true;
    downloadDir = "/tank/data/media";
    mountDependencies = [
      "tank-data.mount"
      "setup-media-dir.service"
    ];
    extraGroups = [ "media-shared" ];
  };
}
