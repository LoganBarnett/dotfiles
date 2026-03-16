{ pkgs ? import <nixpkgs> {}, lib, makeWrapper, stdenv }:

stdenv.mkDerivation {
  pname = "blocky-lists-updater";
  version = "2025-01-19";

  src = pkgs.fetchFromGitHub {
    owner = "shizunge";
    repo = "blocky-lists-updater";
    rev = "e6742a505c620fed0007b5100db67dd1d292436d";
    sha256 = "01x4gs14x0chvpzmz22hd0dr7bdxsapdsf57rc33iy8wijvdqm36";
  };

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin $out/lib
    cp -r src/* $out/lib/

    makeWrapper $out/lib/entrypoint.sh $out/bin/blocky-lists-updater \
      --prefix PATH : ${lib.makeBinPath [
        pkgs.bash
        pkgs.busybox
        pkgs.curl
        pkgs.inotify-tools
        pkgs.static-web-server
      ]}
  '';

  meta = with lib; {
    description = "Update lists without restarting blocky DNS";
    homepage = "https://github.com/shizunge/blocky-lists-updater";
    license = licenses.gpl3;
  };
}
