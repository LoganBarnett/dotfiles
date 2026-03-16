{ nix-output-monitor, writeShellApplication, ... }:
writeShellApplication {
  name = "image-create";
  runtimeInputs = [ nix-output-monitor ];
  text = builtins.readFile ../scripts/image-create;
}
