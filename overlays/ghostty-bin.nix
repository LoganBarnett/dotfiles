################################################################################
# ghostty-bin overlay to use version and hash from static.nix.
#
# Ghostty releases updates independently of nixpkgs.  This lets us track
# the latest release via scripts/ghostty-update without a full nixpkgs bump.
#
# Only overrides the Darwin build; on Linux, ghostty is built from source
# via pkgs.ghostty, not ghostty-bin.
################################################################################
final: prev:
if !prev.stdenv.hostPlatform.isDarwin then
  { }
else
  let
    statics = (import ../static.nix).ghostty-bin;
    inherit (statics) version hash;
  in
  {
    ghostty-bin = prev.ghostty-bin.overrideAttrs (_: {
      inherit version;
      src = final.fetchurl {
        url = "https://release.files.ghostty.org/${version}/Ghostty.dmg";
        name = "Ghostty.dmg";
        inherit hash;
      };
    });
  }
