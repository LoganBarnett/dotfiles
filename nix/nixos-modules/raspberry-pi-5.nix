##
# Provides settings needed to build a Raspberry Pi 5 bootable image.
##
{ flake-inputs }: { ... }: {
  imports = [
    flake-inputs.raspberry-pi-nix.nixosModules.raspberry-pi
    ({ pkgs, ... }: {
      # Note: uboot not yet supported for pi5.  See:
      # https://github.com/tstat/raspberry-pi-nix/issues/13#issuecomment-2090601812
      # # bcm2711 for rpi 3, 3+, 4, zero 2 w
      # bcm2712 for rpi 5
      # See the docs at:
      # https://www.raspberrypi.com/documentation/computers/linux_kernel.html#native-build-configuration
      raspberry-pi-nix = {
        uboot.enable = false;
        # Required for RPi5.
        board = "bcm2712";
        libcamera-overlay.enable = false;
        firmware-migration-service.enable = true;
        # Also GPIO pins?
        pin-inputs.enable = false;
        # The GPIO pins.
        # i2c = false;
      };
    })
  ];
}
