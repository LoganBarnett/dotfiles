{ pkgs ? import <nixpkgs> {}, lib }:
pkgs.stdenv.mkDerivation rec {
  name = "battlescribe-update-data";
  src = ./battlescribe-update-data.sh;

  installPhase = ''
    cp ${src} $out/${name}
  '';
  postPatch = ''
    substituteInPlace ${name}.sh \
      --replace "latestversion" "${pkgs.latestversion}/bin/latestversion"
  '';
}
