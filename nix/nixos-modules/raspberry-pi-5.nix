##
# Provides settings needed to build a Raspberry Pi 5 bootable image.
##
{ flake-inputs, ... }: {
  imports = [
    ./raspberry-pi-host.nix
    # flake-inputs.nixos-hardware.nixosModules.raspberry-pi-5
    flake-inputs.nixos-raspberrypi.nixosModules.raspberry-pi-5.base
    flake-inputs.nixos-raspberrypi.nixosModules.raspberry-pi-5.page-size-16k
  ];
  boot.loader.raspberryPi.bootloader = "kernel";
  # Undocumented from nixos-raspberrypi.  See
  # https://github.com/nvmd/nixos-raspberrypi/blob/f8900910f63477626010938da8d849ba2cc3d011/modules/configtxt-config.nix
  # for now this gets laid down.
  hardware.raspberry-pi = {
    config.all.options = {
      # The USB connection can hypothetically suspend and cause prints to fail.
      # By default this is 2, but setting it to -1 disables it.
      "usbcore.autosuspend" = {
        enable = true;
        value = "-1";
      };
    };
  };
}
