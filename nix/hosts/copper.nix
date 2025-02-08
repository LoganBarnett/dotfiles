################################################################################
# This defines the entirety of the configuration for the copper host.
#
# Copper runs Wireguard.
# nix build '.#nixosConfigurations.copper.config.system.build.sdImage' --show-trace
################################################################################
{ flake-inputs, host-id, system, ... }: let
  wireguard-port = 51820;
in {
  imports = [
    ../nixos-modules/nix-builder-provide.nix
    ../nixos-modules/server-host.nix
    # TODO: Right now agenix-rekey wants to build wireguard to do the
    # generation.  This fails due to a problem with macOS building wireguard-go
    # (documented in the overlay in this repository).  It is not understood why
    # it's not using the overlays.  I've removed the root nixpkgs channel and it
    # still provides this error, even with all the settings see in
    # ../nixos-modules/nix-flake-environment.nix.  I'm at a loss here.  This
    # probably warrants a ticket and I'll get to that sometime.  I have
    # confirmed that commenting this out (at least temporarily) fixes the issue.
    # (import ../nixos-modules/wireguard-server.nix {
    #   inherit host-id;
    # })
    ({ pkgs, ... }: {
      # networking.hostId is needed by the filesystem stuffs.
      # An arbitrary ID needed for zfs so a pool isn't accidentally imported on
      # a wrong machine (I'm not even sure what that means).  See
      # https://search.nixos.org/options?channel=24.05&show=networking.hostId&from=0&size=50&sort=relevance&type=packages&query=networking.hostId
      # for docs.
      # Get from an existing machine using:
      # head -c 8 /etc/machine-id
      # Generate for a new machine using:
      # head -c4 /dev/urandom | od -A none -t x4 | tr -d ' '
      networking.hostId = "d88d6477";
      nixpkgs.hostPlatform = system;
    })
  ];
}
