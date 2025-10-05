################################################################################
# Trivia: Gallium is used for non-toxic thermometers as an alternative to
# mercury, due to its low melting point.  It also is a critical element needed
# for making violet and blue LEDs.  Gallium can be 3D printed.
#
# Gallium is an authentication server.
################################################################################
{ flake-inputs, host-id, system, ... }: let
in {
  imports = [
    # ../nixos-configs/authentik.nix
    ../nixos-modules/raspberry-pi-5.nix
    ../nixos-modules/server-host.nix
    ../nixos-configs/jenkins-github-webhook-test.nix
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
    })
  ];
}
