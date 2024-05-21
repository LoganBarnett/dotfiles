# Works! But it's a mess, and definitely not something that would pass peer
# review. I need to ask what can be done to do this more the "nix" way, and
# hopefully it can stand as a simplistic example.
{ pkgs ? import <nixpkgs> {}, lib }:
pkgs.stdenv.mkDerivation {
  name = "blueutil";
  src = pkgs.fetchgit {
    url = "https://github.com/toy/blueutil.git";
    rev = "90a4fdc4cfd5fb33bc6d8e39483b122a46194b60";
    sha256 = "sha256-x2khx8Y0PolpMiyrBatT2aHHyacrQVU/02Z4Dz9fBtI=";
  };

  buildInputs = [] ++ (
    lib.optionals pkgs.stdenv.isDarwin [
      # We need cf-private because blueutil brings in private symbols.
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
