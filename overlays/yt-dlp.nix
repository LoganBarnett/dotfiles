################################################################################
# yt-dlp overlay to use version and hash from static.nix.
#
# yt-dlp needs frequent updates to keep up with YouTube changes.  This lets
# us update independently of nixpkgs via scripts/yt-dlp-update.
################################################################################
final: prev:
let
  statics = (import ../static.nix).yt-dlp;
  inherit (statics) version hash;
in
{
  yt-dlp = prev.yt-dlp.overrideAttrs (old: {
    inherit version;
    src = final.fetchFromGitHub {
      owner = "yt-dlp";
      repo = "yt-dlp";
      rev = version;
      inherit hash;
    };
    # nixpkgs postPatch patches a curl_cffi version guard that was removed in
    # newer yt-dlp releases; clear it to avoid build failures on version bumps.
    postPatch = "";
    propagatedBuildInputs = (old.propagatedBuildInputs or [ ]) ++ [
      # ffmpeg is required to mux separate video and audio streams; without it
      # only pre-muxed (lower quality) formats are available.
      final.ffmpeg
      # Deno is required to solve YouTube's n-challenge; without it yt-dlp
      # produces empty files for most YouTube content.
      final.deno
    ];
  });
}
