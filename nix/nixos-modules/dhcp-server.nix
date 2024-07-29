################################################################################
# This configures a DHCP server.
#
# Much of this comes from
# https://github.com/ghostbuster91/blogposts/blob/a2374f0039f8cdf4faddeaaa0347661ffc2ec7cf/router2023-part2/main.md
# - initial searches do not reveal anything on the NixOS wiki.
################################################################################
{ host-id, interface }: { ... }: {
  boot.kernel = {
    # By default the Linux kernel drops all packets that are not destined for
    # its interfaces.  This changes that so we can get DHCP client broadcasts.
    sysctl = {
      "net.ipv4.conf.all.forwarding" = true;
      "net.ipv6.conf.all.forwarding" = false;
    };
  };
  systemd.network = {
    # Without this, networkd activation would fail because would be waiting
    # until timeout is reached for all managed interfaces to come online.
    # Since there's only one interface, I don't think this matters?  The
    # tutorial had a mutli-NIC device.
    # wait-online.anyInterface = true;
    networks = {
      # How does one determine this name?
      "30-${interface}" = {
        # Does this name matter or is it arbitrary?
        matchConfig.Name = interface;
        linkConfig.RequiredForOnline = "enslaved";
        networkConfig = {
          ConfigureWithoutCarrier = true;
        };
      };
      # Make routing on this interface a dependency for network-online.target.
      # Actually we probably don't need this since we're not standing up a
      # router.
      # linkConfig.RequiredForOnline = "routable";
    };
  };
  services.dnsmasq = {
    enable = true;
    settings = let
      security-settings = {
        # Prevent bogus private reverse lookups from happening.  A bogus private
        # address is one that this server doesn't know about.
        bogus-priv = true;
        # Do not forward A or AAA requests without a domain or domain part.  An
        # answer to such a request would be wrong or an attack.
        domain-needed = true;
        # Don't use /etc/hosts as this would advertise this host as localhost.
        no-hosts = true;
        # Do not use /etc/resolv.conf for doing lookups.  Only use what is
        # provided here.
        no-resolv = true;
      };
      performance-settings = {
        # Cache dns queries.
        cache-size = 1000;
      };
      network-prefix = "192.168.10";
    in security-settings // performance-settings // {
      dhcp-range = [
        "${interface},${network-prefix}.50,${network-prefix}.254,24h"
      ];
      inherit interface;
      dhcp-host = "${network-prefix}.1";
      # This is what we consider to be a local domain, if the domain segment
      # matches this.  Should use this ^ and $?
      local = "/proton/";
      # This expands domainless queries to be suffixed with ".proton" when
      # combined with `expand-hosts = true;`.
      domain = "proton";
      expand-hosts = true;
      # This is our host's IP, since we aren't using /etc/hosts.
      address = "/${host-id}.proton/${network-prefix}.1";
    };
  };
}
