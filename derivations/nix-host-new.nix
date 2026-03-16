{ writeShellApplication }:
writeShellApplication {
  name = "nix-host-new";
  text = builtins.readFile ../scripts/nix-host-new;
}
