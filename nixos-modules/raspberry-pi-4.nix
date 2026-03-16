##
# Provides settings needed to build a Raspberry Pi 4 image.
##
{ flake-inputs, ... }: {
  imports = [
    flake-inputs.nixos-raspberrypi.nixosModules.raspberry-pi-4.base
    flake-inputs.nixos-raspberrypi.nixosModules.raspberry-pi-4.display-vc4
    ./raspberry-pi-host.nix
    # This section grants us the vcgencmd command and the ability to query the
    # voltage state (as well as checking for under voltage throttling).  This
    # only exists in RPi4 and earlier.  It also gives us access to the Pi's GPU.
    ({ lib, pkgs, ... }: {
      boot.kernelModules = [ "vchiq" ];
      environment.systemPackages = [ pkgs.libraspberrypi ];
    })
  ];
  boot.loader.raspberryPi.bootloader = "uboot";
}
