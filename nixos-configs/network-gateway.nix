################################################################################
# Toggleable network gateway module.
#
# When imported on a host, that host becomes the network's router/gateway.
# Combines VLAN sub-interfaces, NAT, inter-VLAN firewall, and per-VLAN DHCP
# into a single file that replaces the old silicon-vlans.nix, silicon-nat.nix,
# and silicon-vlan-firewall.nix.
#
# To activate:  Import this file in the host's config (e.g. silicon.nix).
# To deactivate: Remove/comment the import.  Everything reverts to the
#                consumer-router-as-gateway topology.
################################################################################
{
  config,
  facts,
  host-id,
  lib,
  ...
}:
let
  networkInterface = facts.network.hosts.${host-id}.networkInterface;
  subnet = facts.network.subnets.barnett-main;
in
{
  # 1. Declare this host as the gateway via DNS alias.
  networking.dnsAliases = [ "gateway" ];

  # 2. VLAN sub-interfaces on the physical trunk.
  networking.vlans = {
    main = {
      id = 10;
      interface = networkInterface;
    };
    iot = {
      id = 20;
      interface = networkInterface;
    };
    guest = {
      id = 30;
      interface = networkInterface;
    };
    wan = {
      id = 100;
      interface = networkInterface;
    };
  };

  # 3. IP addressing — physical NIC has no IP, VLANs do.
  networking.interfaces.${networkInterface}.ipv4.addresses = lib.mkForce [ ];
  networking.interfaces.main.ipv4.addresses = [
    {
      address = "${subnet}.${toString facts.network.hosts.${host-id}.ipv4}";
      prefixLength = 24;
    }
  ]
  ++ (lib.mapAttrsToList (_: ipv4: {
    address = "${subnet}.${toString ipv4}";
    prefixLength = 32;
  }) (facts.network.hosts.${host-id}.extraAddresses or { }));
  networking.interfaces.iot.ipv4.addresses = [
    {
      address = "${facts.network.subnets.barnett-iot}.1";
      prefixLength = 24;
    }
  ];
  networking.interfaces.guest.ipv4.addresses = [
    {
      address = "${facts.network.subnets.barnett-guest}.1";
      prefixLength = 24;
    }
  ];
  networking.interfaces.wan.useDHCP = true;

  # 4. This host IS the gateway — default route from WAN DHCP.
  #    Remove static defaultGateway that points at the old consumer router.
  networking.defaultGateway = lib.mkForce null;

  # 5. NAT: masquerade all internal VLANs + WireGuard out through WAN.
  #    externalInterface overrides wireguard's mkDefault, so iptables
  #    in wireguard/server-standard.nix automatically use "wan".
  boot.kernel.sysctl."net.ipv4.conf.all.forwarding" = true;
  networking.nat.externalInterface = "wan";
  networking.nat.internalInterfaces = [
    "main"
    "iot"
    "guest"
  ];

  # 6. VLAN isolation firewall.
  networking.nftables.enable = true;
  networking.nftables.tables.vlan-isolation = {
    family = "inet";
    content = ''
      chain forward {
        type filter hook forward priority filter; policy drop;
        ct state established,related accept
        iifname "main" accept
        iifname "guest" oifname "wan" accept
        # IoT: blocked by default. Per-device allowlist:
        #   iifname "iot" ip saddr 10.20.0.X oifname "wan" accept
      }
    '';
  };

  # 7. Override DHCP to serve per-VLAN ranges.
  services.dnsmasq.settings = {
    interface = lib.mkForce [
      "main"
      "iot"
      "guest"
    ];
    bind-interfaces = true;
    dhcp-range = lib.mkForce [
      "192.168.254.175,192.168.254.250,12h"
      "tag:iot,10.20.0.100,10.20.0.250,12h"
      "tag:guest,10.30.0.100,10.30.0.250,1h"
    ];
    dhcp-option = lib.mkForce [
      "option:domain-search,${facts.network.domain}"
      "option:router,${subnet}.${toString facts.network.hosts.${host-id}.ipv4}"
      "option:dns-server,${subnet}.${toString facts.network.hosts.${host-id}.ipv4}"
      "tag:iot,option:router,${facts.network.subnets.barnett-iot}.1"
      "tag:iot,option:dns-server,${facts.network.subnets.barnett-iot}.1"
      "tag:guest,option:router,${facts.network.subnets.barnett-guest}.1"
      "tag:guest,option:dns-server,${facts.network.subnets.barnett-guest}.1"
    ];
  };

  # 8. Remove multi-interface static IP config (VLANs replace it).
  #    dhcp-server.nix sets IPs on enp3s0/eno1/eth0/etc — not needed when
  #    the gateway module puts IPs on VLAN interfaces instead.
  networking.interfaces =
    lib.genAttrs [ "enp3s0" "eno1" "end0" "ens0" "eth0" ]
      (_: {
        ipv4.addresses = lib.mkForce [ ];
      });
}
