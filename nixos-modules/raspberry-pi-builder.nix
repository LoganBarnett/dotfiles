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
    # Begin Nix cache settings so we needn't build the Linux kernel.
    # Building the kernel has been problematic - the Pi we've been building this
    # against (nickel.proton) runs for at least an hour or two and then the host
    # becomes unresponsive in any form, and has required a hard power cycle.
    # I'd prefer to build everything from source if possible.
    # When trying this via the linux-builder, we get a problem with BTS and
    # vmlinux (this is documented further in ../linux-kernel.org).  Like the Pi,
    # it might be a resource issue (more than 8GB of RAM might be required, for
    # example).  I've not done higher resource limited attempts yet.
    # That said, none of this works because nothing is up there right now.  I
    # suspect this relates to
    # https://github.com/nix-community/raspberry-pi-nix/issues/8#issuecomment-1954410029
    # wherein @tstat mentions that the cache is more or less manually managed as
    # part of the release process.
    # I'd prefer not to be using the cache anyways and instead manage my own, so
    # it would be nice to just get this figured out.
    # extra-substituters = [ "https://nix-community.cachix.org" ];
    # extra-trusted-public-keys = [
    #   "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    # ];
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
