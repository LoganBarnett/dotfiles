{ git, writeShellApplication }:
writeShellApplication {
  name = "git-outstanding-repos";
  runtimeInputs = [ git ];
  # The script iterates over unquoted command substitutions, which shellcheck
  # flags as potentially unsafe word splitting.
  checkPhase = "";
  text = builtins.readFile ../scripts/git-outstanding-repos;
}
