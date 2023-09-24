{ lib, pkgs, ... }:
{
  # Somehow this can get lost, and I'm not convinced this is home-managers'
  # nor nix's doing. That said, this setting seems to have no effect.
  # networking.hostname = "neon.proton";
  # I tried setting this in flake.nix as well. See comment block there for
  # more details.
  # config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
  #   "hello-unfree" # For testing unfree-ness.
  #   "ngrok"
  #   "unrar"
  # ];
}
