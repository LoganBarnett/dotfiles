################################################################################
# zinc provides completely seamless VPN tunnel by acting as a WiFi access point
# that bridges all of its traffic to the proton network via Wireguard's VPN.
# This communicates directly with the copper host.
#
# zinc runs Wireguard as a client and
# nix build '.#nixosConfigurations.zinc.config.system.build.sdImage' --show-trace
################################################################################
{ disko-proper, flake-inputs, nixpkgs }: let
  host-id = "zinc";
  system = "aarch64-linux";
  wireguard-port = 51820;
in {
  imports = [
    # We don't want builds going over the tunnel to this host.
    # ../nixos-modules/nix-builder-provide.nix
    (import ../nixos-modules/server-host.nix {
      inherit flake-inputs host-id system;
    })
    # (import ../nixos-modules/wireguard-client.nix {
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
      networking.hostId = "f8732ff6";
      nixpkgs.hostPlatform = system;
    })
    ({ config, ... }: {
      age.secrets."${host-id}-wireguard-client" = {
        script = "wireguard-priv";
        rekeyFile = ../secrets/${host-id}-wireguard-client;
      };
      # TODO: Switch to netdevs.
      # The Wireguard NixOS wiki lacks a lot of context even if it has a lot of
      # information.  The documentation for the option to enable wireguard via
      # this networking.wireguard stuff is recommended _not_ to be used and
      # instead netdevs is to be used.  I need to look into that, as it seems to
      # make more sense when using Wireguard purely as a client.
      networking.wireguard.interfaces = {
        wg0 = {
          ips = [];
          listenPort = wireguard-port;
        };
      };
    })
  ];
}
