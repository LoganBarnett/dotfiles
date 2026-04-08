################################################################################
# MakeMKV 1.17.7 — binary-only package for firmware flashing.
#
# MakeMKV 1.18.x has a bug where `makemkvcon f` (the firmware tool) spins at
# 100 % CPU and never actually flashes.  This derivation extracts just the
# prebuilt makemkvcon binary from the 1.17.7 bin tarball and patches it for
# NixOS, linking against the current MakeMKV's shared libraries.
#
# Usage:
#   makemkvcon f --all-yes -d /dev/sr0 -f sdf.bin rawflash enc -i firmware.bin
#
# Update the version and hash in static.nix when upstream fixes the bug and
# this package is no longer needed.
################################################################################
{
  autoPatchelfHook,
  fetchurl,
  glibc,
  lib,
  makemkv,
  stdenv,
}:
let
  statics = (import ../../static.nix).makemkv-flasher;
  inherit (statics) version;
in
stdenv.mkDerivation {
  pname = "makemkv-flasher";
  inherit version;

  src = fetchurl {
    urls = [
      "http://www.makemkv.com/download/makemkv-bin-${version}.tar.gz"
      "http://www.makemkv.com/download/old/makemkv-bin-${version}.tar.gz"
    ];
    inherit (statics.bin) hash;
  };

  nativeBuildInputs = [ autoPatchelfHook ];

  # The prebuilt makemkvcon links against libmakemkv and libdriveio.  We
  # satisfy those from the current MakeMKV package — the firmware tool ABI has
  # been stable across minor versions.
  buildInputs = [
    makemkv
    glibc
  ];

  # The tarball ships a Makefile that tries to run /bin/bash for EULA
  # acceptance.  We skip the build entirely and just install the binary.
  dontBuild = true;
  dontConfigure = true;

  installPhase = ''
    runHook preInstall
    install -Dm555 bin/amd64/makemkvcon "$out/bin/makemkvcon"
    runHook postInstall
  '';

  meta = {
    description = "MakeMKV ${version} firmware flash tool (workaround for 1.18.x flashing bug)";
    homepage = "https://www.makemkv.com";
    platforms = [ "x86_64-linux" ];
    license = lib.licenses.unfree;
  };
}
