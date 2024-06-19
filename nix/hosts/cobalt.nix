################################################################################
# This defines the entirety of the configuration for the cobalt host.
#
# The cobalt host is currently tasked as a git collaboration server.  This is
# like a self-hosted GitHub or Bitbucket.
################################################################################
{ disko-proper, flake-inputs, build-system, nixpkgs }: let
  gitea-port = 3000;
  host-id = "cobalt";
  system = "aarch64-linux";
in {
  inherit system;
  specialArgs = {
    inherit flake-inputs;
  };
  modules = [
    # ../nixos-modules/raspberry-pi-disk.nix
    # flake-inputs.nixos-generators.nixosModules.all-formats
    # "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-raspberrypi.nix"
    # "${nixpkgs}/nixos/modules/installer/sd-card/sd-image.nix"
    "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
    ({ config, lib, pkgs, ... }: {
      # nixpkgs.hostPlatform = "aarch64-linux";
      # nixpkgs.hostPlatform = "aarch64-unknown-linux-gnu";
      # boot.kernelPackages = lib.mkForce pkgs.linuxKernel.packages.linux_rpi4;
      services.openssh.hostKeys = [
        {
          path = config.age.secrets."${host-id}-pub-key".path;
          type = "ed25519";
        }
      ];
    })
    ../nixos-modules/nix-builder-provide.nix
    (import ../nixos-modules/server-host.nix {
      inherit disko-proper flake-inputs host-id system;
    })
    (import ../nixos-modules/https.nix {
      inherit host-id;
      listen-port = 443;
      server-port = gitea-port;
      fqdn = "${host-id}.proton";
    })
    ../nixos-modules/gitea-server.nix
  ]
  ++ (if build-system == "raspberry-pi-nix" then [
    flake-inputs.raspberry-pi-nix.nixosModules.raspberry-pi
  ] else [])
  ;
} // (if build-system == "raspberry-pi-nix" then {
} else {})
