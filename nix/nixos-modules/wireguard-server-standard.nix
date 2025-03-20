################################################################################
# Uses the "standard" networking seen in the NixOS wiki here:
# https://wiki.nixos.org/wiki/WireGuard
# I don't know what word better than "standard" would work.  The other is using
# networkd.  I tried getting networkd working, but I can't even get Wireguard to
# show me its configuration.
################################################################################
{ config, host-id, lib, pkgs, ... }: let
  wireguard-port = 51820;
  vpn-subnet-prefix = "192.168.102";
  network-interface = "enu1u1";
  peers = [
    { host-id = "scandium"; ip = "20"; }
    # Unsure how to force these - I think I'd just generate them manually in the
    # case of a non-Nix managed device (like a phone).
    { host-id = "manganese"; ip = "22"; }
    { host-id = "selena-laptop"; ip = "23"; }
    # agenix-rekey's generators always put in a newline that fouls up the
    # generation here.  I could trim the newlines like is done in the
    # ./ldap-server.nix module, or I could just fix the problem at its source.
    # This requires some rebasing and updating of agenix-rekey, which doesn't go
    # cleanly for me.  I'm up against the most recent working commit (the next
    # commit breaks things for me).  For now I'm leaving these hosts commented
    # out and focusing on the one I manually edited.
    # { host-id = "zinc"; ip = "23"; }
  ];
  wireguard-client-peer = { host-id, ip }: {
    # This demands the actual key and not a path.  Use the ./. idiom to get the
    # path but also put this file in the nix store where we can get to it.
    publicKey = builtins.readFile ../secrets/${host-id}-wireguard-client.pub;
    allowedIPs = [ "${vpn-subnet-prefix}.${ip}/32" ];
  };
  wireguard-client-secret = host-id: {
    "${host-id}-wireguard-client" = {
      generator.script = "wireguard-priv";
      rekeyFile = ../secrets/${host-id}-wireguard-client.age;
    };
  };
in {
  age.secrets = {
    "${host-id}-wireguard-server" = {
      generator.script = "wireguard-priv";
      rekeyFile = ../secrets/${host-id}-wireguard-server.age;
    };
  }
  // (lib.attrsets.mergeAttrsList
    (builtins.map wireguard-client-secret (builtins.map (p: p.host-id) peers))
  );
  imports = [
    ./wireguard-agenix-rekey-generator.nix
  ];
  environment.systemPackages = [
    # Allow us to run Wireguard commands to show configuration and diagnose
    # issues.
    pkgs.wireguard-tools
  ];
  networking.nat.enable = true;
  networking.nat.externalInterface = network-interface;
  networking.nat.internalInterfaces = [ "wg0" ];
  networking.firewall = {
    allowedUDPPorts = [ 51820 ];
  };

  networking.wireguard.enable = true;
  networking.wireguard.interfaces = {
    # "wg0" is the network interface name. You can name the interface arbitrarily.
    wg0 = {
      # Determines the IP address and subnet of the server's end of the tunnel
      # interface.
      ips = [ "${vpn-subnet-prefix}.1/24" ];

      # The port that WireGuard listens to. Must be accessible by the client.
      listenPort = 51820;

      # This allows the wireguard server to route your traffic to the Internet
      # and hence be like a VPN For this to work you have to set the dnsserver
      # IP of your router (or dnsserver of choice) in your clients.
      postSetup = ''
        ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s ${vpn-subnet-prefix}.0/24 -o ${network-interface} -j MASQUERADE
      '';

      # This undoes the above command.
      postShutdown = ''
        ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s ${vpn-subnet-prefix}.0/24 -o ${network-interface} -j MASQUERADE
      '';


      privateKeyFile = config.age.secrets."${host-id}-wireguard-server".path;
      peers = (builtins.map wireguard-client-peer peers);
      # peers = [
      #   # List of allowed peers.
      #   { # Feel free to give a meaning full name
      #     # Public key of the peer (not a file path).
      #     publicKey = "{client public key}";
      #     # List of IPs assigned to this peer within the tunnel subnet. Used to configure routing.
      #     allowedIPs = [ "10.100.0.2/32" ];
      #   }
      #   { # John Doe
      #     publicKey = "{john doe's public key}";
      #     allowedIPs = [ "10.100.0.3/32" ];
      #   }
      # ];
    };
  };
}
