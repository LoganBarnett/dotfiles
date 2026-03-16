{ writeShellApplication }:
writeShellApplication {
  name = "get-ip";
  # The script was authored as zsh and uses echo escape sequences that
  # shellcheck flags under bash wrapping.
  checkPhase = "";
  text = builtins.readFile ../scripts/get-ip;
}
