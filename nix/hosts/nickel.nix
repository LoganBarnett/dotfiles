################################################################################
# This defines the entirety of the configuration for the nickel host.
#
# Nickel is tasked as an LDAP server to declare access and permission.
#
# Use this to build:
# nix build '.#nixosConfigurations.nickel.config.system.build.sdImage' --show-trace
################################################################################
{ facts, flake-inputs, host-id, nodes, system, ... }: let
in {
  imports = [
    ../nixos-modules/dns-server.nix
    ../nixos-modules/raspberry-pi-4.nix
    ../nixos-modules/nix-builder-provide.nix
    ../nixos-modules/raspberry-pi-builder.nix
    ../nixos-modules/server-host.nix
    ../nixos-modules/ldap-server.nix
    ../nixos-modules/prometheus-server.nix
    ../nixos-modules/grafana.nix
    # (import ../nixos-modules/freeipa-server.nix { inherit host-id; })
    # Pi stuff.
    ({ pkgs, ... }: {
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
    # {
    #   containers.grafana = {
    #     autoStart = true;
    #     privateNetwork = false;
    #     specialArgs = {
    #       inherit facts flake-inputs nodes system;
    #       host-id = "grafana";
    #     };
    #     config = import ./grafana.nix;
    #   };
    # }
    # (import ../nixos-modules/sd-image-raspberrypi.nix {
    #   inherit flake-inputs;
    # })
    # (import ../nixos-modules/hardware-raspberry-pi-main.nix {
    #   version = "4";
    #   model = "B";
    #   inherit flake-inputs;
    # })
    # "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
  ];
}
