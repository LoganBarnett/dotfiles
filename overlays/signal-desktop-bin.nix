################################################################################
# Disable the expiration "feature" in Signal Desktop.  Updating Signal Desktop
# requires updating all of nixpkgs (even if just for one package via multiple
# inputs).  If this were infrequent it would be more tolerable, but it expires
# about every week or two.  My only recourse is to disable the code directly.
#
# The signal-desktop package in nixpkgs only builds for Linux.  The Darwin
# version is `signal-desktop-bin`, which just downloads and unpacks a `dmg`
# image.
################################################################################
final: prev: prev.signal-desktop-bin.overrideAttrs (old: {
  preInstall = final.lib.traceVal ''
    mkdir src-modified
    rm "$src/Signal.app/Contents/Resources/app.asar.unpacked/app.asar.unpacked"
    ${prev.asar}/bin/asar
      unpack
      "$src/Signal.app/Contents/Resources/app.asar.unpacked/app.asar"
      src-modified
    ls -al src-modified
    # Let's just see what's inside.
    exit 1
  '';
  installPhase = ''
  runHook preInstall
exit 1
  '';
})
