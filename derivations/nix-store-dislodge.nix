{
  callPackage,
  flock,
  lib,
  lsof,
  stdenv,
  writeShellApplication,
}:
let
  nix-store-diagnose = callPackage ./nix-store-diagnose.nix { };
in
writeShellApplication {
  name = "nix-store-dislodge";
  runtimeInputs = [
    flock
    nix-store-diagnose
  ]
  ++ lib.optionals stdenv.isDarwin [ lsof ];
  text = builtins.readFile ../scripts/nix-store-dislodge;
}
