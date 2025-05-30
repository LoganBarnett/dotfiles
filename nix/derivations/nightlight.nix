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

})
