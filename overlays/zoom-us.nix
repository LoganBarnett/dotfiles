################################################################################
# zoom-us overlay to use version and hash from static.nix.
#
# Zoom releases updates frequently without a stable API for version detection.
# This lets us update independently of nixpkgs via scripts/zoom-us-update.
#
# Only overrides the Darwin build; on Linux, zoom-us is a buildFHSEnv wrapper
# whose inner version cannot be cleanly replaced via overrideAttrs.
################################################################################
final: prev:
if !prev.stdenv.hostPlatform.isDarwin then
  { }
else
  let
    statics = (import ../static.nix).zoom-us;
    inherit (statics) version hash;
  in
  {
    zoom-us = prev.zoom-us.overrideAttrs (_: {
      inherit version;
      src = final.fetchurl {
        url = "https://zoom.us/client/${version}/zoomusInstallerFull.pkg?archType=arm64";
        name = "zoomusInstallerFull.pkg";
        inherit hash;
      };
    });
  }
