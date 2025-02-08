################################################################################
# This defines the entirety of the configuration for the cobalt host.
#
# The cobalt host is currently tasked as a git collaboration server.  This is
# like a self-hosted GitHub or Bitbucket.
################################################################################
{ flake-inputs, system, nixpkgs, ... }: let
  gitea-port = 3000;
  host-id = "cobalt";
  system = "aarch64-linux";
in {
  imports = [
    ../nixos-modules/raspberry-pi-5.nix
    ({ config, lib, pkgs, ... }: {
      nixpkgs.hostPlatform = system;
      # nixpkgs.hostPlatform = "aarch64-unknown-linux-gnu";
      # boot.kernelPackages = lib.mkForce pkgs.linuxKernel.packages.linux_rpi4;
      # This isn't working due to a bootstrapping issue.  Do not use.
      # services.openssh.hostKeys = [
      #   {
      #     path = config.age.secrets."${host-id}-pub-key".path;
      #     type = "ed25519";
      #   }
      # ];
    })
    ../nixos-modules/nix-builder-provide.nix
    ../nixos-modules/server-host.nix
    (import ../nixos-modules/https.nix {
      inherit host-id;
      listen-port = 443;
      server-port = gitea-port;
      fqdn = "${host-id}.proton";
    })
    ../nixos-modules/gitea-server.nix
  ];
}
