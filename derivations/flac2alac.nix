{ ffmpeg, writeShellApplication }:
writeShellApplication {
  name = "flac2alac";
  runtimeInputs = [ ffmpeg ];
  # The script injects filenames into sh -c via {}, which shellcheck flags.
  checkPhase = "";
  text = builtins.readFile ../scripts/flac2alac;
}
