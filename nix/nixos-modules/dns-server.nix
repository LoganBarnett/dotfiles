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
{ lib, facts, ... }: {
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
        (lib.attrsets.mapAttrsToList (hostname: host: (let
          # TODO: Make this a dynamic value on the host.
          subnet = facts.network.subnets.barnett-main;
        in
          "${hostname},${subnet}.${toString host.ipv4}"
        )))
      ];
      # dhcp-host = [
      #   # Examples - make these dynamic.
      #   "nickel,192.168.100.10"
      #   "grafana,192.168.100.20"
      #   "prometheus,192.168.100.30"
      # ];
      dhcp-option = [
        "option:router,192.168.254.254"
        "option:dns-server,192.168.254.254"
      ];
    };
  };
}
