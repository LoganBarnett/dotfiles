{ imagemagick, writeShellApplication }:
writeShellApplication {
  name = "heic2png";
  runtimeInputs = [ imagemagick ];
  text = builtins.readFile ../scripts/heic2png;
}
