{ nixos-rebuild, writeShellApplication }:
writeShellApplication {
  name = "proton-deploy";
  runtimeInputs = [ nixos-rebuild ];
  text = builtins.readFile ../scripts/proton-deploy;
}
