{ writeShellApplication }:
writeShellApplication {
  name = "tty-reset";
  # The script passes an unquoted $1 to a redirect target, which shellcheck
  # flags as SC2086.
  checkPhase = "";
  text = builtins.readFile ../scripts/tty-reset;
}
