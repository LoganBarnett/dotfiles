################################################################################
# Gallium hosts our NextCloud server - essentially an all-in-one service for
# file sharing, calendar support, and a number of other features that are nice
# to have in one place.  This is called a "groupware" I think.
#
# Trivia: Gallium is used for non-toxic thermometers as an alternative to
# mercury, due to its low melting point.  It also is a critical element needed
# for making violet and blue LEDs.  Gallium can be 3D printed.
################################################################################
{ flake-inputs, host-id, system, ... }: let
in {
  imports = [
    ../nixos-modules/raspberry-pi-5.nix
    ../nixos-modules/nix-builder-provide.nix
    ../nixos-modules/server-host.nix
    ../nixos-modules/nextcloud.nix
    # (import ../nixos-modules/dhcp-server.nix {
    #   inherit host-id;
    #   interface = "eth0";
    # })
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
