{
  direnv,
  git,
  writeShellApplication,
  ...
}:
writeShellApplication {
  name = "nix-direnv-add-envrc";
  runtimeInputs = [
    direnv
    git
  ];
  text = builtins.readFile ../scripts/nix-direnv-add-envrc.sh;
}
