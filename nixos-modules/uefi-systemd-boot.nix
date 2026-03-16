################################################################################
# Enable UEFI booting for this host using systemd-boot.
################################################################################
{ ... }: {
  boot = {
    initrd.kernelModules = [
      # These can be necessary to make sure the keyboard is
      # available in the UEFI boot menu.
      "usbhid"
    ];
    loader.efi = {
      canTouchEfiVariables = true;
    };
    loader.systemd-boot = {
      enable = true;
      # Generally we have 100MB /boot partitions (which is suggested
      # in a lot of places, I want to say), but this will cause NixOS
      # to fill this up and then `nixos-switch build` starts failing
      # or at least starts giving errors.  Ignoring errors are bad.
      # Set this to 5 and that's all NixOS will keep as a "restore"
      # boot point for us.
      configurationLimit = 5;
      # Default is true for backwards compatibility, but the recommended is
      # to set to false.
      # But setting it to true allows editing the startup options using "e"
      # which is very helpful for debugging.
      editor = true;
      memtest86.enable = true;
    };
  };
}
