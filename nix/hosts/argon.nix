################################################################################
# argon is the wireguard host.
# argon used to be an OctoPi server running octo-print.
################################################################################
{ flake-inputs, host-id, ... }: let
  system = "aarch64-linux";
in {
  imports = [
    (import ../nixos-modules/raspberry-pi-4.nix {
      inherit flake-inputs;
    })
    ../nixos-modules/raspberry-pi-host.nix
    ../nixos-modules/nix-builder-provide.nix
    ../nixos-modules/server-host.nix
    ../nixos-modules/wireguard-server-standard.nix
  ];
  # networking.hostId is needed by the filesystem stuffs.
  # An arbitrary ID needed for zfs so a pool isn't accidentally imported on
  # a wrong machine (I'm not even sure what that means).  See
  # https://search.nixos.org/options?channel=24.05&show=networking.hostId&from=0&size=50&sort=relevance&type=packages&query=networking.hostId
  # for docs.
  # Get from an existing machine using:
  # head -c 8 /etc/machine-id
  # Generate for a new machine using:
  # head -c4 /dev/urandom | od -A none -t x4
  networking.hostId = "c1042166";
  nixpkgs.hostPlatform = system;
}
