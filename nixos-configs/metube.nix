################################################################################
# Metube is a web frontend for yt-dlp.  Paste a URL in the UI and the media
# lands in /tank/data/media where Jellyfin can pick it up.
################################################################################
{ pkgs, ... }:
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
    # Pinned to a fork carrying a fix for the multiprocessing.Manager
    # subprocess dying and leaving Download.manager pointed at a dead socket,
    # which silently bricked all downloads until a service restart.  Upstream
    # PR pending; drop this override once the fix is in a tagged release.
    #
    # Only the outer derivation's src is overridden — the frontend
    # sub-derivation captures the upstream src via let-binding and is built
    # unchanged, which is fine because the fix is backend-only.
    package = pkgs.metube.overrideAttrs (_: {
      src = pkgs.fetchFromGitHub {
        owner = "LoganBarnett";
        repo = "metube";
        rev = "9a0ed3aa743e19f32f210edc1b56661507b06558";
        hash = "sha256-7bnR64IcKT18myS0ZwYIR3tqDY7+xSmSXR3EumEIGoU=";
      };
    });
  };
}
