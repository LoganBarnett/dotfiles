{ rage, writeShellApplication, ... }:
writeShellApplication {
  name = "nix-host-key-install";
  runtimeInputs = [ rage ];
  text = builtins.readFile ../bin/nix-host-key-install;
}
