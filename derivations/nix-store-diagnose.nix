{
  flock,
  lib,
  lsof,
  stdenv,
  writeShellApplication,
}:
writeShellApplication {
  name = "nix-store-diagnose";
  runtimeInputs = [ flock ] ++ lib.optionals stdenv.isDarwin [ lsof ];
  text = builtins.readFile ../scripts/nix-store-diagnose;
}
