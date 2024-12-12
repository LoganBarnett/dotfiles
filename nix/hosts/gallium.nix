################################################################################
# Gallium provides some basic network functionality via DHCP as well as some
# tooling to help with network detection and testing.  For example, it hosts an
# nginx instance with both an HTTP and HTTPS endpoints that can be tested.
#
# Trivia: Gallium is used for non-toxic thermometers as an alternative to
# mercury, due to its low melting point.  It also is a critical element needed
# for making violet and blue LEDs.  Gallium can be 3D printed.
################################################################################
{ disko-proper, flake-inputs }: { ... }: let
  host-id = "gallium";
  system = "aarch64-linux";
in {
  imports = [
    (import ../nixos-modules/raspberry-pi-5.nix {
      inherit flake-inputs;
    })
    # (import ../nixos-modules/dhcp-server.nix {
    #   inherit host-id;
    #   interface = "eth0";
    # })
    ../nixos-modules/nix-builder-provide.nix
    (import ../nixos-modules/server-host.nix {
      inherit flake-inputs host-id system;
    })
    ({ lib, pkgs, ... }: {
      # networking.hostId is needed by the filesystem stuffs.
      # An arbitrary ID needed for zfs so a pool isn't accidentally imported on
      # a wrong machine (I'm not even sure what that means).  See
      # https://search.nixos.org/options?channel=24.05&show=networking.hostId&from=0&size=50&sort=relevance&type=packages&query=networking.hostId
      # for docs.
      # Get from an existing machine using:
      # head -c 8 /etc/machine-id
      # Generate for a new machine using:
      # head -c4 /dev/urandom | od -A none -t x4 | tr -d ' '
      networking.hostId = "f9daf086";
      nixpkgs.hostPlatform = system;

      documentation.enable = lib.mkForce false;
    })
  ];
}
