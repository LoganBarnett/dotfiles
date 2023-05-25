{ lib, pkgs, ... }:
{
  # Some packages are not "free". We need to specifically bless those.
  allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "ngrok"
    "unrar"
  ];
  # Somehow this can get lost, and I'm not convinced this is home-managers'
  # nor nix's doing. That said, this setting seems to have no effect.
  # networking.hostname = "neon.proton";
  permittedInsecurePackages = [
    "openssl-1.0.2u"
  ];
}
