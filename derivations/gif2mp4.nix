{ ffmpeg, writeShellApplication }:
writeShellApplication {
  name = "gif2mp4";
  runtimeInputs = [ ffmpeg ];
  # The script passes unquoted positional parameters to ffmpeg.
  checkPhase = "";
  text = builtins.readFile ../scripts/gif2mp4;
}
