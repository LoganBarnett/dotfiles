################################################################################
# An installer for Raspberry Pis.  Not so much intended directly as an
# installer, but something to give us to debug a host and possibly
# configure/update its firmware.
################################################################################
{ flake-inputs, pkgs, system, ... }: {
  imports = [
    # 5 is backwards compatible with 4.
    ../nixos-modules/raspberry-pi-4.nix
    ../nixos-modules/server-host.nix
  ];
  environment.systemPackages = [
    pkgs.raspberrypi-eeprom
  ];
}
