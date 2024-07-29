################################################################################
# Configures this Raspberry Pi as a Raspberry Pi builder.
#
# This may not actually be necessary but I've done some snooping around and I'm
# not sure how much aarch64-darwin really applies to Raspberry Pis if one wants
# to get full functionality out of the hardware (such as BlueTooth and GPIO).
#
# Much of this was experimental and I'm just saving it for reference in case I
# come back to it.
################################################################################
{ ... }: let
  # Isn't it actually one of these though?  It identifies as aarch64-darwin when
  # running.
  # system = "armv6l-linux";
  # system = "armv7l-linux";
in {
  nix.distributedBuilds = true;
  boot.binfmt.emulatedSystems = [
    "armv6l-linux"
    "armv7l-linux"
  ];
  nix.settings = {
    extra-platforms = [
      "armv6l-linux"
      "armv7l-linux"
    ];
  };
  # nixpkgs.hostPlatform = "aarch64-unknown-linux-gnu";
  # Workaround for issue?
  # programs.command-not-found.enable = false;
  # raspberry-pi-nix specific:
  # boot.kernelPackages = pkgs.rpi-kernels.v5_15_87.kernel;
  # boot.kernelPackages = pkgs.rpi-kernels.v6_6_28.kernel;
  # boot.kernelPackages = pkgs.rpi-kernels.v6_1_63.kernel;
  # raspberry-pix-nix.pin-kernel = false;
  # (import ../nixos-modules/sd-image-raspberrypi.nix {
  #   inherit flake-inputs;
  # })
  # (import ../nixos-modules/hardware-raspberry-pi-main.nix {
  #   version = "4";
  #   model = "B";
  #   inherit flake-inputs;
  # })
  # "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
  # ../nixos-modules/raspberry-pi-disk.nix
}
