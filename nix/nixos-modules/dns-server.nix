################################################################################
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
  # Ugh I'm not sure how to make this work with my ISP's DNS.  Do I even want
  # that?
  networking.nameservers = let
    # Just so we know whose these are.
    cloudflare = "1.1.1.1";
    opendns = "208.67.222.222";
    quad9 = "9.9.9.9";
    # I'd rather give Google the tracking capability than have no DNS.  But the
    # other three failing is pretty unlikely.
    google = "8.8.8.8";
  in [
    # Order is intentional.
    opendns
    quad9
    cloudflare
    google
  ];
  # Allow actual DNS and DHCP connections.
  networking.firewall.allowedUDPPorts = [ 53 67 ];
  # Larger connections (DNSSEC, zone transfers) use TCP for DNS.
  networking.firewall.allowedTCPPorts = [ 53 ];
  services.dnsmasq = {
    enable = true;
    settings = {
      domain = facts.network.domain;
      local = "/${facts.network.domain}/";
      expand-hosts = true;
      dhcp-range = "192.168.254.100,192.168.254.200,12h";
      dhcp-host = lib.pipe facts.network.hosts [
        (lib.attrsets.filterAttrs (hostname: host:
          host ? ipv4 && host.ipv4 != null
        ))
        (lib.attrsets.mapAttrsToList (hostname: host:
          "${hostname},${host-ip hostname host}"
        ))
      ];
      # dhcp-host = [
      #   # Examples - make these dynamic.
      #   "nickel,192.168.100.10"
      #   "grafana,192.168.100.20"
      #   "prometheus,192.168.100.30"
      # ];
      dhcp-option = [
        "option:router,192.168.254.254"
        "option:dns-server,${my-ip}"
      ];
    };
  };
}
