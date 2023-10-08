# Works! But it's a mess, and definitely not something that would pass peer
# review. I need to ask what can be done to do this more the "nix" way, and
# hopefully it can stand as a simplistic example.
{ pkgs ? import <nixpkgs> {}, lib }:
pkgs.stdenv.mkDerivation {
  name = "blueutil";
  src = pkgs.fetchgit {
    url = "https://github.com/toy/blueutil.git";
    sha256 = "/HjrZ3lab1SxSn/aEcT+CYIJYVJteimP59Yjy/2zsys=";
  };

  buildInputs = [] ++ (
    lib.optionals pkgs.stdenv.isDarwin [
      pkgs.darwin.cf-private
      pkgs.darwin.apple_sdk_11_0.frameworks.IOBluetooth
    ]
  );

  # Not sure how to get this tied in.
  # makeFlags = [
  #   "CFLAGS=-Wall -Wextra -Werror -mmacosx-version-min=10.9 -framework Foundation -framework IOBluetooth"
  # ];

  # Flags lifted straight from the Makefile.
  buildPhase = ''
  cc -Wall -Wextra -Werror -mmacosx-version-min=10.9 -framework Foundation -framework IOBluetooth blueutil.m -o blueutil
'';

  installPhase = ''
mkdir -p $out/bin
cp blueutil $out/bin
  '';

}
