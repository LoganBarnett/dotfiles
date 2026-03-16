{ writeShellApplication, ... }:
writeShellApplication {
  name = "nix-darwin-switch";
  text = builtins.readFile ../scripts/nix-darwin-switch.sh;
}
