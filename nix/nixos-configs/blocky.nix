################################################################################
# Maybe you've heard of Pi-Hole and that's cool and all but I wanted something
# declarative for blocking domains, and that's Blocky.  This is for blocking
# domains such as ad servers, malware, and so on.  I also get fine grained
# control to block specific hosts or groups of hosts from looking up groups of
# domains.  This makes it really easy to manage standard things I want blocked
# (like ads and malware) and some people get blocked more (like kids from adult
# sites, gaming, social media, etc).

# This reads heavily from ../nixos-modules/facts.nix.
#
# Blocky needs to stand in front of the entire DNS setup in order to work
# properly, since Blocky sees the DNS request coming from a client, and decides
# how to respond based on that client's definition in Blocky.  If Blocky were
# later in the DNS chain (like dnsmasq came first), then Blocky would only see
# dnsmasq talking to it, and we'd lose Blocky's power to block what we wanted by
# specific hosts or groups of hosts.
################################################################################
{ config, facts, host-id, lib, pkgs, ... }: let
  inherit (lib) optionals optionalAttrs pipe;
  inherit (lib.attrsets) mapAttrs' mapAttrsToList;
  inherit (lib.lists) any flatten filter unique;
  # Just so we know whose these are.
  cloudflare = "1.1.1.1";
  opendns = "208.67.222.222";
  quad9 = "9.9.9.9";
  # I'd rather give Google the tracking capability than have no DNS.  But the
  # other three failing is pretty unlikely.
  google = "8.8.8.8";
  nameservers = [
    # Order is intentional.
    opendns
    quad9
    cloudflare
    google
  ];
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
  # TODO: Make dynamic some day.  Make it follow "gateway".
  networking.defaultGateway = "${subnet}.254";
  # Allow actual DNS and DHCP connections.
  networking.firewall.allowedUDPPorts = [ 53 ];
  # Larger connections (DNSSEC, zone transfers) use TCP for DNS.
  networking.firewall.allowedTCPPorts = [ 53 ];
  services.https.fqdns."blocky.proton" = {
    enable = true;
    internalPort = config.services.blocky.settings.ports.http;
  };
  services.blocky = {
    enable = true;
    settings = {
      ports = { dns = 53; http = 4000; };
      upstream = { default = [ "192.168.254.1" ]; };
      # Look up hosts by hostname from dnsmasq, which is providing DHCP.
      clientLookup = {
        upstream = "${subnet}.1";
        singleNameOrder = [ 1 ];
        # You actually want `blocking.denylists` instead of `clients`.
        # clients = [];
      };
      blocking = {
        # Supposedly this is `denylists` in the new version, but it doesn't work
        # on the current version.
        blackLists = {
          ads = [
            # This one includes malware too, and I haven't found one that is
            # just ads.
            "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"
            "https://big.oisd.nl/domainswild"
            "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"
            "https://blocklistproject.github.io/Lists/ads.txt"
            "https://blocklistproject.github.io/Lists/tracking.txt"
          ];
          adult = [
            "https://blocklistproject.github.io/Lists/porn.txt"
          ];
          gaming = [
            # "https://raw.githubusercontent.com/blocklistproject/Lists/master/gaming.txt"
          ];
          # Includes anything that could be an attack or scam.
          malware = [
            # This one includes ads too, and I haven't found one that is just
            # malware.
            "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"
            "https://blocklistproject.github.io/Lists/abuse.txt"
            "https://blocklistproject.github.io/Lists/crypto.txt"
            "https://blocklistproject.github.io/Lists/fraud.txt"
            "https://blocklistproject.github.io/Lists/phishing.txt"
            "https://blocklistproject.github.io/Lists/piracy.txt"
            "https://blocklistproject.github.io/Lists/ransomware.txt"
            "https://blocklistproject.github.io/Lists/scam.txt"
          ];
          social = [
            # "https://blocklistproject.github.io/Lists/social.txt"
            "https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/social-only/hosts"
          ];
          video = [
            # "https://blocklistproject.github.io/Lists/streaming.txt"
            "https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/fakenews/hosts"
            # "https://raw.githubusercontent.com/nextdns/native-tracking-domains/main/streaming.txt"
          ];
        };
        # Supposedly this is `allowlists` in the new version, but it doesn't work
        # on the current version.
        # whiteLists = { ads = [ "allowlist.txt" ]; };
        clientGroupsBlock = let
          # For a yuck-moment:  This is sort of magical list.  Because these
          # names resolve to a profile name, Blocky won't treat this as a
          # hostname.
          profileGroups = {
            adult = [ "ads" "malware" ];
            child = [ "ads" "adult" "gaming" "malware" ];
            # TODO: Consider making a guest profile, wherein only a select
            # allow list is used.  All hosts either use this unless they are
            # declared somewhere via facts.  This can help some entities from
            # dialing home.
            default = [ "ads" "adult" "gaming" "malware" ];
          };
          # TODO: The current approach is flawed in that it assumes that the
          # only way to declare the presence of a host is via the hosts
          # declaration, but users can have devices which indicate IPs to use
          # for the VPN.  What we should probably do is move it such that the
          # host itself declares what sort of VPN IPs it can have and have the
          # rest of the machinery follow.  This should be more preferable than
          # having to check a user's device list.
          # blockProfilesForHost = otherHostId: pipe facts.network.users [
          #   (filter (u: any (d: d.host-id == otherHostId) u.devices))
          #   (map (u: u.blockProfiles or []))
          #   flatten
          #   unique
          # ];
          profilesByHosts = pipe facts.network.hosts [
            (mapAttrsToList (name: data: let
              blockProfiles = data.blockProfiles or [ "child" ];
            in [
              {
                name = "${name}.${facts.network.domain}";
                value = blockProfiles;
              }
              {
                inherit name;
                value = blockProfiles;
              }
            ] ++ (optionals (data.ipv4 or null != null) [{
              name = "${facts.network.subnets.barnett-main}.${toString data.ipv4}";
              value = blockProfiles;
            }]) ++ (optionals (data.macs or null != null) [{
              name = data.mac;
              value = blockProfiles;
            }])
            ))
            flatten
            builtins.listToAttrs
          ];
        in profileGroups // profilesByHosts;
      };
      prometheus.enable = true;
    };
  };
}
