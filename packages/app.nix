{ pkgs, ... }:
pkgs.writeShellApplication {
  name = "app";
  text = builtins.readFile ./app.sh;
}
