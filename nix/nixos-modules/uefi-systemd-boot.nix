################################################################################
# Enable UEFI booting for this host using systemd-boot.
################################################################################
{ ... }: {
  boot = {
    loader.efi = {
      canTouchEfiVariables = true;
    };
    loader.systemd-boot = {
      enable = true;
      # Default is true for backwards compatibility, but the recommended is
      # to set to false.
      # But setting it to true allows editing the startup options using "e"
      # which is very helpful for debugging.
      editor = true;
      memtest86.enable = true;
    };
  };
}
