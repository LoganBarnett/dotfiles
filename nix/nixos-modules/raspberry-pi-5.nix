##
# Provides settings needed to build a Raspberry Pi 5 bootable image.
##
{ flake-inputs }: { ... }: {
  imports = [
    flake-inputs.raspberry-pi-nix.nixosModules.raspberry-pi
    ({ lib, pkgs, ... }:
    #   let
    #   linux_rpi5 = pkgs.linux_rpi4.override {
    #     rpiVersion = 5;
    #     argsOverride.defconfig = "bcm2712_defconfig";
    #   };
    # in {
      {
        # imports = [
        #   (import ../nixos-modules/hardware-raspberry-pi-main.nix {
        #       inherit flake-inputs;
        #       version = "5";
        #   })
        # ];
        # boot.kernelParams = [ "dtb=\\bcm2712-rpi-5-b.dtb" ];
      # boot.kernelPackages = pkgs.linuxPackages_rpi5;
      # boot.kernelPackages = lib.mkForce (pkgs.linuxPackagesFor linux_rpi5);
      # Note: uboot not yet supported for pi5.  See:
      # https://github.com/tstat/raspberry-pi-nix/issues/13#issuecomment-2090601812
      # # bcm2711 for rpi 3, 3+, 4, zero 2 w
      # bcm2712 for rpi 5
      # See the docs at:
      # https://www.raspberrypi.com/documentation/computers/linux_kernel.html#native-build-configuration
      raspberry-pi-nix = {
        # Leave disabled to fix boot issues per:
        # https://github.com/nix-community/raspberry-pi-nix/issues/16
        # There doesn't seem to be strong benefit to keeping it on anyways,
        # since it's kind of a pain to reboot to a prior generation.
        uboot.enable = false;
        # Required for RPi5.
        board = "bcm2712";
        # The GPIO pins.
        # i2c = false;
        # libcamera-overlay.enable = false;
        # firmware-migration-service.enable = true;
        # Also GPIO pins?
        # pin-inputs.enable = false;
      };
      hardware = {
        # bluetooth.enable = true;
        raspberry-pi = {
          config = {
            all = {
              # base-dt-params = {
              #   # enable autoprobing of bluetooth driver
              #   # https://github.com/raspberrypi/linux/blob/c8c99191e1419062ac8b668956d19e788865912a/arch/arm/boot/dts/overlays/README#L222-L224
              #   krnbt = {
              #     enable = true;
              #     value = "on";
              #   };
              # };
            };
          };
        };
      };
    })
  ];
}
