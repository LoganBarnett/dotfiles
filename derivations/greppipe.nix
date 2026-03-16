{ gnugrep, writeShellApplication }:
writeShellApplication {
  name = "greppipe";
  runtimeInputs = [ gnugrep ];
  # The script checks $? directly via test, which shellcheck flags as SC2181.
  checkPhase = "";
  text = builtins.readFile ../scripts/greppipe;
}
