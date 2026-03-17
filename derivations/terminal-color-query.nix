{ python3, writeShellApplication }:
writeShellApplication {
  name = "terminal-color-query";
  runtimeInputs = [ python3 ];
  text = builtins.readFile ../scripts/terminal-color-query;
}
