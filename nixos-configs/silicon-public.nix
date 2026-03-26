################################################################################
# Public/private nginx split for silicon.
#
# Private services (*.proton) listen on 192.168.254.9, which the router never
# forwards externally.  Public services (future real TLDs) will listen on
# 192.168.254.100, which the router port-forwards TCP 443 → .100.
#
# The nftables rule below provides defense-in-depth: even if the router is
# misconfigured to forward traffic to .9, non-RFC-1918 source addresses are
# dropped at the host firewall before reaching nginx.  The custom table runs
# at priority -1 so it fires before the nixos-fw chain at priority 0; this
# ensures our DROP takes precedence over nixos-fw's ACCEPT for allowed ports.
################################################################################
{ ... }:
{
  imports = [
    ../nixos-modules/upnp-portforward.nix
  ];

  services.https.domains = {
    "proton" = {
      addr = "192.168.254.9";
      certSource = "internal-ca";
    };
    "logustus.com" = {
      addr = "192.168.254.100";
      certSource = "acme";
    };
    "meshward.com" = {
      addr = "192.168.254.100";
      certSource = "acme";
    };
  };

  # Keep the public-facing port-forward registration alive via UPnP.
  services.upnp-portforward = {
    enable = true;
    addr = "192.168.254.100";
    externalPort = 443;
    localPort = 443;
  };

  # Switch the NixOS firewall to the nftables backend so the custom table
  # below is handled by the same stack.
  networking.nftables.enable = true;

  # Defense-in-depth: drop non-RFC-1918 traffic aimed at the private IP.
  # Uses a distinct table name to avoid conflicts with nixos-fw and to support
  # additive merging via types.lines if other modules contribute their own
  # table definitions.
  networking.nftables.ruleset = ''
    table inet silicon-private-guard {
      chain input {
        type filter hook input priority -1; policy accept;
        ip daddr 192.168.254.9 ip saddr != { 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16 } tcp dport 443 drop
      }
    }
  '';
}
