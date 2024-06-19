################################################################################
# This is the required hardware settings for any Raspberry Pi device.
# https://wiki.nixos.org/wiki/NixOS_on_ARM/Raspberry_Pi for reference.
################################################################################
{ flake-inputs, version, model }: {
  imports = [
    flake-inputs.nixos-hardware.nixosModules."raspberry-pi-${version}"
    # Infinite recursion happens when this is used this way.
    # flake-inputs.nixos-hardware.nixosModules.raspberry-pi-4
  ];
  nixpkgs.overlays = [
    (final: prev: {
      # Work https://github.com/NixOS/nixpkgs/issues/154163 in which we get the
      # error:
      # modprobe: FATAL: Module sun4i-drm not found in directory /nix/store/2g3bml1wcnd1jq3amxyrj5gxnsm49q1b-linux-6.1.63-stable_20231123-modules/lib/modules/6.1.63
      makeModulesClosure = x:
        prev.makeModulesClosure (x // { allowMissing = true; });
    })
  ];
  # Ugh, deprecation warnings that documentation uses.
  # TODO: Fix the official docs to represent what options should _really_ be
  # used.
  # boot.loader.raspberryPi.enable = true;
  # # Set the version depending on your raspberry pi.
  # boot.loader.raspberryPi.version = version;
  # # We need uboot
  # boot.loader.raspberryPi.uboot.enable = true;
  # boot.loader.grub.enable = false;
  # boot.loader.generic-extlinux-compatible.enable = true;
  # fileSystems."/" = {
  #   device = "rpool/root/nixos";
  #   fsType = "zfs";
  # };

  # fileSystems."/home" = {
  #   device = "rpool/home";
  #   fsType = "zfs";
  # };

  # fileSystems."/boot" = {
  #   device = "/dev/sda1";
  #   fsType = "vfat";
  # };
  # boot.initrd.supportedFilesystems = [ "zfs" ];
  # boot.supportedFilesystems = [ "zfs" ];
  # boot.initrd.availableKernelModules = [ "xhci_pci" "usbhid" "uas" "usb_storage" ];

}
