################################################################################
# Run a Wireguard server.
################################################################################
({ host-id }: { config, pkgs, ... }: let
  wireguard-port = 51820;
  vpn-subnet-prefix = "192.168.102";
  peers = [
    { host-id = "manganese"; ip = "22"; }
    { host-id = "scandium"; ip = "21"; }
    { host-id = "zinc"; ip = "23"; }
  ];
  wireguard-client-peer = { host-id, ip }: (import ./wireguard-client-peer.nix {
    inherit host-id;
    inherit ip;
    inherit vpn-subnet-prefix;
  });
in {
  age.secrets."${host-id}-wireguard-server" = {
    generator.script = "wireguard-priv";
    rekeyFile = ../secrets/${host-id}-wireguard-server.age;
  };
  imports = (builtins.map wireguard-client-peer peers) ++ [
    ./wireguard-agenix-rekey-generator.nix
  ];
  environment.systemPackages = [
    # Allow us to run Wireguard commands to show configuration and diagnose
    # issues.
    pkgs.wireguard-tools
  ];
  networking = {
    firewall = {
      allowedUDPPorts = [ wireguard-port ];
    };
    useNetworkd = true;
  };
  systemd.network = {
    enable = true;
    netdevs = {
      # What is the relevance of the number?  I believe it is to control load
      # order.
      "50-wg" = {
        netdevConfig = {
          Kind = "wireguard";
          Name = "wg0";
          # Magic value?  Why this?
          MTUBytes = "1300";
        };
        wireguardConfig = {
          PrivateKeyFile = config.age.secrets."${host-id}-wireguard-server".path;
          ListenPort = wireguard-port;
          # wg-quick creates routing entries automatically but we must use use
          # this option in systemd.
          RouteTable = "main";
        };
      };
    };
    networks.wg0 = {
      matchConfig.Name = "wg0";
      address = [ "${vpn-subnet-prefix}.1/24" ];
      networkConfig = {
        # Uh, what?  Doesn't this need to just route?
        IPMasquerade = "ipv4";
        IPv4Forwarding = true;
        IPv6Forwarding = true;
      };
    };
  };
  # TODO: Remove?
  # networking.nat.enable = true;
  # networking.nat.externalInterface = "enu1u1";
  # networking.nat.internalInterfaces = [ "wg0" ];
  # networking.wireguard.interfaces = let
  #   lan-subnet = "192.168.254.1/24";
  #   vpn-subnet = "${vpn-subnet-prefix}.1/24";
  # in {
  #   # "wg0" is the network interface name.  You can name the interface
  #   # arbitrarily.
  #   wg0 = {
  #     # Determines the IP address and subnet of the server's end of the
  #     # tunnel interface.
  #     ips = [
  #       "${vpn-subnet-prefix}.1/32"
  #     ];
  #     # The port that WireGuard listens to.  Must be accessible by the
  #     # client.
  #     listenPort = wireguard-port;
  #     # This allows the wireguard server to route your traffic to the
  #     # internet and hence be like a VPN For this to work you have to set
  #     # the dnsserver IP of your router (or dnsserver of choice) in your
  #     # clients.
  #     postSetup = ''
  #           ${pkgs.iptables}/bin/iptables \
  #             -t nat \
  #             -A POSTROUTING \
  #             -s ${vpn-subnet-prefix}.0/24 \
  #             -o eth0 \
  #             -j MASQUERADE
  #         '';

  #     # This undoes the above command on shutdown of the systemd service.
  #     postShutdown = ''
  #           ${pkgs.iptables}/bin/iptables \
  #             -t nat \
  #             -D POSTROUTING \
  #             -s ${vpn-subnet-prefix}.0/24 \
  #             -o eth0 \
  #             -j MASQUERADE
  #         '';

  #     # Path to the private key file.
  #     #
  #     # Note: The private key can also be included inline via the privateKey
  #     # option, but this makes the private key world-readable; thus, using
  #     # privateKeyFile is recommended.
  #     privateKeyFile = config.age.secrets."${host-id}-wireguard".path;
  #   };
  # };
})
