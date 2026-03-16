{ rage, writeShellApplication, ... }:
writeShellApplication {
  name = "nix-host-key-install";
  runtimeInputs = [ rage ];
  text = builtins.readFile ../scripts/nix-host-key-install;
}
