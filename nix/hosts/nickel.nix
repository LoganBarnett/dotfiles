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
  # Isn't it actually one of these though?  It identifies as aarch64-darwin when
  # running.
  # system = "armv6l-linux";
  # system = "armv7l-linux";
  system = "aarch64-linux";
in {
  # inherit system;
  imports = [
    ../nixos-modules/nix-builder-provide.nix
    # Pi stuff.
    ({ pkgs, ... }: {
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
      # networking.hostId is needed by the filesystem stuffs.
      # An arbitrary ID needed for zfs so a pool isn't accidentally imported on
      # a wrong machine (I'm not even sure what that means).  See
      # https://search.nixos.org/options?channel=24.05&show=networking.hostId&from=0&size=50&sort=relevance&type=packages&query=networking.hostId
      # for docs.
      # Get from an existing machine using:
      # head -c 8 /etc/machine-id
      # Generate for a new machine using:
      # head -c4 /dev/urandom | od -A none -t x4
      networking.hostId = "027a9bda";
      nixpkgs.hostPlatform = system;
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
      inherit flake-inputs host-id system;
    })
    (import ../nixos-modules/ldap-server.nix { inherit host-id; })
    ../nixos-modules/raspberry-pi-disk.nix
  ];
}
