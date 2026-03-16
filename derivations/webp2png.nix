{ ffmpeg, writeShellApplication }:
writeShellApplication {
  name = "webp2png";
  runtimeInputs = [ ffmpeg ];
  text = builtins.readFile ../scripts/webp2png;
}
