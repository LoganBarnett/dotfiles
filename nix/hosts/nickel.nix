################################################################################
# This defines the entirety of the configuration for the nickel host.
#
# Nickel is tasked as an LDAP server to declare access and permission.
#
# Use this to build:
# nix build '.#nixosConfigurations.nickel.config.system.build.sdImage' --show-trace
################################################################################
{ disko-proper, flake-inputs, nixpkgs }: let
  host-id = "nickel";
  system = "aarch64-linux";
in {
  # inherit system;
  imports = [
    ../nixos-modules/nix-builder-provide.nix
    # Pi stuff.
    ({ pkgs, ... }: {
      # nixpkgs.hostPlatform = system;
      # nixpkgs.hostPlatform = "aarch64-unknown-linux-gnu";
      # Workaround for issue?
      # programs.command-not-found.enable = false;
      # raspberry-pi-nix specific:
      # boot.kernelPackages = pkgs.rpi-kernels.v5_15_87.kernel;
      # boot.kernelPackages = pkgs.rpi-kernels.v6_6_28.kernel;
      # boot.kernelPackages = pkgs.rpi-kernels.v6_1_63.kernel;
      # raspberry-pix-nix.pin-kernel = false;
    })
    # (import ../nixos-modules/sd-image-raspberrypi.nix {
    #   inherit flake-inputs;
    # })
    # (import ../nixos-modules/hardware-raspberry-pi-main.nix {
    #   version = "4";
    #   model = "B";
    #   inherit flake-inputs;
    # })
    # "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
    (import ../nixos-modules/server-host.nix {
      inherit disko-proper flake-inputs host-id;
    })
    (import ../nixos-modules/ldap-server.nix { inherit host-id; })
    ({ ... }: {
      nixpkgs.hostPlatform = system;
    })
  ];
}
