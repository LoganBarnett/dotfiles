{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "pair-ls";
  src = pkgs.fetchurl {
    # TODO: Make this OS agnostic.
    url = "https://github.com/stevearc/pair-ls/releases/download/v0.1.1/pair-ls-mac";
    sha256 = "a10lepIvC7mHcrEs5yypPYhmVzaXul7e/lpWeapA1z4=";
  };
  phases = ["installPhase" "patchPhase"];
  installPhase = ''
    mkdir -p $out/bin
    cp $src $out/bin/pair-ls
    chmod +x $out/bin/pair-ls
  '';
}
