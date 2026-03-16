################################################################################
# This configures a DHCP server.
#
# Due to a combination of just having a lot of physical hosts available as well
# as employing containers to better utilize those physical hosts, we need some
# kind of DNS solution that extends beyond a consumer router that provides DNS
# for only physical hosts.
#
# This solution expects that DNS is no longer provided by the consumer router,
# and so this must be disabled.
#
# Because we're on Nix and we can statically define everything, there is no need
# for dynamic DNS on the part of the server for any of our known hosts.  We can
# simply allow the facts structure to dictate what we need.
################################################################################
{ lib, facts, host-id, ... }: let
  inherit (lib) optionalString pipe;
  inherit (lib.attrsets) filterAttrs mapAttrsToList;
  inherit (lib.lists) flatten;
  domain = facts.network.domain;
  # TODO: Make this a dynamic value on the host.
  subnet = facts.network.subnets.barnett-main;
  host-ip = hostname: host: "${subnet}.${toString host.ipv4}";
  my-ip = host-ip host-id facts.network.hosts.${host-id};
  # This is because the names vary.  Instead of trying to guess, just set them
  # all.
  forced-ip-interface-config = {
    ipv4.addresses = [{
      address = my-ip;
      prefixLength = 24;
    }];
  };
in {
  networking.interfaces = {
    # systemd "predictable".
    enp3s0 = forced-ip-interface-config;
    eno1 = forced-ip-interface-config;
    # Firmware set.
    end0 = forced-ip-interface-config;
    ens0 = forced-ip-interface-config;
    # Sometimes user forced.
    eth0 = forced-ip-interface-config;
  };
  # TODO: Make dynamic some day.  Make it follow "gateway".
  networking.defaultGateway = "${subnet}.254";
  # Allow actual DNS and DHCP connections.
  networking.firewall.allowedUDPPorts = [ 67 ];
  services.dnsmasq = {
    enable = true;
    settings = {
      # Use non-standard DNS port to avoid conflict with blocky on port 53.
      # Blocky will query this port for local hostname resolution.
      port = 5353;
      # Private addresses that don't resolve anything should return NXDOMAIN
      # lest we get record pollution elsewhere, I guess?
      bogus-priv = true;
      domain = facts.network.domain;
      local = "/${facts.network.domain}/";
      # Prevent dnsmasq from reading /etc/hosts.  We don't want this file read,
      # because it often contains a localhost entry (such as 127.0.0.2) - or at
      # least that's how NixOS sets it up.
      no-hosts = true;
      expand-hosts = true;
      dhcp-range = "192.168.254.175,192.168.254.250,12h";
      dhcp-host = pipe facts.network.hosts [
        (filterAttrs (hostname: host:
          host ? ipv4 && host.ipv4 != null
        ))
        (mapAttrsToList (hostname: host:
          (optionalString (host.macAddress or null != null) "${host.macAddress or ""},")
          + "${hostname},${host-ip hostname host}"
        ))
      ];
      host-record = pipe facts.network.hosts [
        (filterAttrs (hostname: host:
          host ? ipv4 && host.ipv4 != null
        ))
        (mapAttrsToList (hostname: host:
          "${hostname}.${domain},${host-ip hostname host}"
        ))
      ];
      cname = lib.pipe facts.network.hosts [
        (mapAttrsToList (hostname: host:
          builtins.map
            (alias: "${alias}.${domain},${hostname}.${domain}")
            (host.aliases or [])
        ))
        flatten
      ];
      # dhcp-host = [
      #   # Examples - make these dynamic.
      #   "nickel,192.168.100.10"
      #   "grafana,192.168.100.20"
      #   "prometheus,192.168.100.30"
      # ];
      log-dhcp = true;
      dhcp-option = [
        "option:domain-search,${domain}"
        "option:router,192.168.254.254"
        "option:dns-server,192.168.254.${
          toString facts.network.hosts.silicon.ipv4
        }"
      ];
    };
  };

  # Goss health checks for dnsmasq DHCP and local DNS.
  services.goss.checks = {
    # Verify dnsmasq is running.
    service.dnsmasq = {
      enabled = true;
      running = true;
    };
    # Verify dnsmasq is on non-standard DNS port 5353 (not 53 which is used by
    # blocky).
    port."udp:5353" = {
      listening = true;
    };
    # Verify dnsmasq is on DHCP port.
    port."udp:67" = {
      listening = true;
    };
    # Functional test: Query dnsmasq directly for local hostname resolution.
    # Verifies dnsmasq can resolve the current host's FQDN.
    dns."${host-id}.${facts.network.domain}" = {
      resolvable = true;
      server = "127.0.0.1:5353";
      timeout = 3000;
    };
  };
}

################################################################################
# A stub taken from the wiki.
# Much of this comes from
# https://github.com/ghostbuster91/blogposts/blob/a2374f0039f8cdf4faddeaaa0347661ffc2ec7cf/router2023-part2/main.md
# - initial searches do not reveal anything on the NixOS wiki.
#
# This is not in use but there might be some learnings to pick apart from it.
################################################################################
# { host-id, interface }: { ... }: {
#   boot.kernel = {
#     # By default the Linux kernel drops all packets that are not destined for
#     # its interfaces.  This changes that so we can get DHCP client broadcasts.
#     sysctl = {
#       "net.ipv4.conf.all.forwarding" = true;
#       "net.ipv6.conf.all.forwarding" = false;
#     };
#   };
#   systemd.network = {
#     # Without this, networkd activation would fail because would be waiting
#     # until timeout is reached for all managed interfaces to come online.
#     # Since there's only one interface, I don't think this matters?  The
#     # tutorial had a mutli-NIC device.
#     # wait-online.anyInterface = true;
#     networks = {
#       # How does one determine this name?
#       "30-${interface}" = {
#         # Does this name matter or is it arbitrary?
#         matchConfig.Name = interface;
#         linkConfig.RequiredForOnline = "enslaved";
#         networkConfig = {
#           ConfigureWithoutCarrier = true;
#         };
#       };
#       # Make routing on this interface a dependency for network-online.target.
#       # Actually we probably don't need this since we're not standing up a
#       # router.
#       # linkConfig.RequiredForOnline = "routable";
#     };
#   };
#   services.dnsmasq = {
#     enable = true;
#     settings = let
#       security-settings = {
#         # Prevent bogus private reverse lookups from happening.  A bogus private
#         # address is one that this server doesn't know about.
#         bogus-priv = true;
#         # Do not forward A or AAA requests without a domain or domain part.  An
#         # answer to such a request would be wrong or an attack.
#         domain-needed = true;
#         # Don't use /etc/hosts as this would advertise this host as localhost.
#         no-hosts = true;
#         # Do not use /etc/resolv.conf for doing lookups.  Only use what is
#         # provided here.
#         no-resolv = true;
#       };
#       performance-settings = {
#         # Cache dns queries.
#         cache-size = 1000;
#       };
#       network-prefix = "192.168.10";
#     in security-settings // performance-settings // {
#       dhcp-range = [
#         "${interface},${network-prefix}.50,${network-prefix}.254,24h"
#       ];
#       inherit interface;
#       dhcp-host = "${network-prefix}.1";
#       # This is what we consider to be a local domain, if the domain segment
#       # matches this.  Should use this ^ and $?
#       local = "/proton/";
#       # This expands domainless queries to be suffixed with ".proton" when
#       # combined with `expand-hosts = true;`.
#       domain = "proton";
#       expand-hosts = true;
#       # This is our host's IP, since we aren't using /etc/hosts.
#       address = "/${host-id}.proton/${network-prefix}.1";
#     };
#   };
# }
