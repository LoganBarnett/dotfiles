{ lib, ... }:
{
  # networking.monitors is flat camelCase for the same reason as
  # networking.dnsAliases — networking.dns is already a list of string
  # (DNS server addresses) so sub-options under it are not possible.
  options.networking.monitors = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [ ];
    description = ''
      Prometheus monitor names for this host.  Each entry is a monitor
      identifier (e.g. "node", "systemd", "goss"); the prometheus-server
      module on the aggregation host reads these to build scrape configs.
      Aggregated across all hosts by nixos-modules/prometheus-server.nix.
    '';
  };
}
