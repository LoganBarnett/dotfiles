{
  gnugrep,
  jq,
  pkgs,
  ...
}: pkgs.writeShellApplication {
  name = "macos-keyboard-remap";
  text = builtins.readFile ./macos-keyboard-remap.sh;
  runtimeInputs = [
    gnugrep
    jq
  ];
}
