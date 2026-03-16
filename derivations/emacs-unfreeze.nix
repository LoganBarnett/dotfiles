{ writeShellApplication }:
writeShellApplication {
  name = "emacs-unfreeze";
  text = builtins.readFile ../scripts/emacs-unfreeze;
}
