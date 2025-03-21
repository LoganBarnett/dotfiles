
{ pkgs ? import <nixpkgs> {}, lib, rustPlatform }:
rustPlatform.buildRustPackage (let
  version = "0.3.0";
in {
  pname = "nightlight";
  inherit version;
  src = pkgs.fetchgit {
    url = "https://github.com/smudge/nightlight.git";
    rev = "v${version}";
    sha256 = "sha256-sj4irWTuB4/j5eoV5dRX43K/tMpLoB0MpPnBaG12To4=";
  };
  cargoHash = "sha256-mdpeo00pvT84ccbTqm2a5Hf8b85l7TajdzKYhkd5Mjg=";
  buildInputs = [] ++ (
    lib.optionals pkgs.stdenv.isDarwin [
      # Are we sure we want to be using 11?
      # pkgs.darwin.apple_sdk_11_0.frameworks.CoreFoundation
      # Documentation indicates that it is more compatible for x86-darwin, but
      # I'm having trouble getting the build to see the private APIs it uses.
      pkgs.darwin.apple_sdk.frameworks.CoreFoundation
    ]
  );

})
