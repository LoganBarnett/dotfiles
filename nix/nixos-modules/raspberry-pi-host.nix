################################################################################
# This is some boilerplate for Raspberry Pi hosts.
#
# We can make some assumptions about building Raspberry Pis since we can create
# the entire bootable image from scratch, and we don't need to deal with UEFI
# nonsense.
################################################################################
{ flake-inputs, host-id, lib, ... }: {
  imports = [
    flake-inputs.nixos-raspberrypi.lib.inject-overlays
    flake-inputs.nixos-raspberrypi.nixosModules.nixpkgs-rpi
    # Make this suspect if there are problems with things like ffmpeg.
    flake-inputs.nixos-raspberrypi.lib.inject-overlays-global
  ];
  # /boot/firmware must be kept small intentionally.
  boot.loader.generic-extlinux-compatible.configurationLimit = 1;
  # boot.loader.grub.enable = false;
  # boot.loader.generic-extlinux-compatible.enable = true;
  # Removed in nixpkgs, but it now comes from nixos-raspberrypi.
  # Unfortunately the options are presently undocumented, so see
  # https://github.com/nvmd/nixos-raspberrypi/blob/f8900910f63477626010938da8d849ba2cc3d011/modules/system/boot/loader/raspberrypi/default.nix
  # for the options.
  boot.loader.raspberryPi = {
    enable = true;
    # This is set differently between 4 and 5.
    # bootloader = "kernel";
  };
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
  # Disable swap devices for these.  They have little SD cards with abysmal
  # write speeds.  Perhaps if we have a permanently connected disk with some
  # speed to it, we can change this up.  We can also destroy the SD cards
  # quickly with a swap device.
  swapDevices = lib.mkForce [];
  # By default, Raspberry Pis are disallowed from doing their own builds.  We
  # have a single Raspberry Pi build host - a host with sufficient memory and no
  # other services on it.  This prevents any local builds from occurring.
  nix.settings.max-jobs = 0;
}
