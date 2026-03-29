{ lib, ... }:
{
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
