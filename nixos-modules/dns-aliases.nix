{ lib, ... }:
{
  # networking.dns is already defined as `list of string` (DNS server
  # addresses) by the nix-darwin networking module, so a nested
  # networking.dns.aliases sub-option is not possible — the module system
  # requires the parent to be an attrset option.  We use flat camelCase
  # instead.
  options.networking.dnsAliases = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [ ];
    description = ''
      DNS CNAMEs pointing at this host.  Each entry is a bare hostname;
      the domain suffix is appended by the DNS server.  Aggregated across
      all hosts by nixos-configs/dhcp-server.nix on the DNS host.
    '';
  };
}
