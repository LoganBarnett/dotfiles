##
# Provides settings needed to build a Raspberry Pi 5 bootable image.
##
{ flake-inputs, ... }: {
  imports = [
    ./raspberry-pi-host.nix
    flake-inputs.nixos-hardware.nixosModules.raspberry-pi-5
  ];
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;
  # This set of labels might not work for fresh images, but it's what the old
  # raspberry-pi-nix `sdImage` output would lay down.  We might need to make
  # this configurable if I ever start emitting new RPi5 images again.  Perhaps
  # move this to its own module at that juncture.
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
    };
    "/boot/firmware" = {
      device = "/dev/disk/by-label/FIRMWARE";
      fsType = "vfat";
    };
  };
}
