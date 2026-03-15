# Enable nix-remote-builder-doctor on Darwin hosts.
{ config, pkgs, ... }:

{
  services.nix-remote-builder-doctor = {
    enable = true;
    builders = [
      { name = "silicon"; hostName = "silicon.proton"; }
      { name = "rpi-build"; hostName = "rpi-build.proton"; }
    ];
  };
}