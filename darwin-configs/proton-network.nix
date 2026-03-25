{
  facts,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ../nixos-modules/lib-custom.nix
  ];
  # macOS per-domain resolver file.  Queries for *.proton are sent to the
  # home network DNS server; all other DNS uses the system resolver
  # unchanged.  This is what allows the split-tunnel WireGuard profile
  # (proton) to resolve .proton hostnames without overriding system DNS.
  environment.etc."resolver/${facts.network.domain}".text = ''
    nameserver ${pkgs.lib.custom.networkDnsIp facts}
  '';
}
