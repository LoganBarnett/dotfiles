################################################################################
# firefox-bin overlay to use version and hash from static.nix.
#
# The nixpkgs firefox-bin derivation includes wrapGAppsHook3 even on Darwin,
# which modifies the Firefox binary and voids Mozilla's code signature.  macOS
# then refuses to run Firefox because the JIT entitlement
# (com.apple.security.cs.allow-jit) no longer validates.
#
# This overlay replaces firefox-bin with a clean stdenvNoCC derivation that
# unpacks the official DMG and copies the .app bundle as-is, preserving the
# original Mozilla signature.  Approach modelled on nixpkgs-firefox-darwin
# (github:bandithedoge/nixpkgs-firefox-darwin).
#
# Use scripts/firefox-bin-update to bump the version without a full nixpkgs
# update.
################################################################################
final: prev:
let
  statics = (import ../static.nix).firefox-bin;
  inherit (statics) version hash;
in
{
  firefox-bin = final.stdenvNoCC.mkDerivation {
    pname = "firefox-bin";
    inherit version;
    src = final.fetchurl {
      url = "https://archive.mozilla.org/pub/firefox/releases/${version}/mac/en-US/Firefox%20${version}.dmg";
      inherit hash;
    };
    nativeBuildInputs = [ final.undmg ];
    sourceRoot = ".";
    # Do not let Nix's fixup phase touch the bundle; any modification breaks
    # Mozilla's notarised code signature.
    dontFixup = true;
    installPhase = ''
      mkdir -p "$out/Applications"
      mv Firefox.app "$out/Applications/Firefox.app"
    '';
    meta = {
      description = "Mozilla Firefox, free web browser (binary package)";
      homepage = "https://www.mozilla.org/firefox/";
      license = {
        shortName = "firefox";
        fullName = "Firefox Terms of Use";
        url = "https://www.mozilla.org/about/legal/terms/firefox/";
        free = false;
        redistributable = true;
      };
      mainProgram = "firefox";
      platforms = [
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      sourceProvenance = [ final.lib.sourceTypes.binaryNativeCode ];
    };
  };
}
