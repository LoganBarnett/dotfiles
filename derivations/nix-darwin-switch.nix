{ pkgs }:
pkgs.writeShellScriptBin "nix-darwin-switch" ''
  sudo darwin-rebuild switch --flake "$FLAKE_ROOT" "$@"
''
