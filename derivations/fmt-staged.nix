{ git, nixfmt, treefmt, writeShellApplication, ... }:
writeShellApplication {
  name = "fmt-staged";
  runtimeInputs = [ git nixfmt treefmt ];
  text = builtins.readFile ../scripts/fmt-staged.sh;
}
