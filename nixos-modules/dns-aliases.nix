{ lib, ... }:
{
  # The nested namespace networking.dns.aliases is intentional.  NixOS
  # convention allows both flat camelCase (networking.dnsAliases) and
  # sub-namespace styles; we use the latter to group cleanly under
  # networking.dns alongside any future DNS-related options.
  options.networking.dns.aliases = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [ ];
    description = ''
      DNS CNAMEs pointing at this host.  Each entry is a bare hostname;
      the domain suffix is appended by the DNS server.  Aggregated across
      all hosts by nixos-configs/dhcp-server.nix on the DNS host.
    '';
  };
}
