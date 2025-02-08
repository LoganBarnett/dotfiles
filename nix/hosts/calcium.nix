################################################################################
# The calcium host.
#
# Its primary use is a Nix cache.  Currently this is achieved via Attic
# (https://github.com/zhaofengli/attic).
#
# Attic doesn't allow for declarative configuration yet, but at least this
# machine can host a lot of disks.  For now, I can use it to back up silicon so
# I can rebuild silicon as a NixOS machine.
#
# History: The machine is a donation from Tom Sears.
#
# Trivia: Calcium is highly reactive metal and rarely found in elemental form.
# There are many nutritional benefits to calcium (bone strength being a common
# one, but they are also part of neurotransmitters).  Calcium is found in many
# things, including limestone.
################################################################################
{ flake-inputs, ... }: let
  host-id = "calcium";
  system = "x86_64-linux";
  atticd-port = 8080;
in {
  imports = [
    (import ../nixos-modules/cache-attic.nix {
      inherit atticd-port;
    })
    (import ../nixos-modules/https.nix {
      listen-port = 443;
      server-port = atticd-port;
      fqdn = "${host-id}.proton";
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
      networking.hostId = "b4a6639e";
      nixpkgs.hostPlatform = system;

      documentation.enable = lib.mkForce false;
    })
    ../nixos-modules/server-host.nix
  ];
}
