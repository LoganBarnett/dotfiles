################################################################################
# A Prometheus client aggregates metrics to be reported to the Prometheus
# server.  Whether the client pushes this data or the server polls for it is
# both something configurable and not something I know the default of.
################################################################################
{ config, lib, facts, host-id, ... }: let
  # monitors = lib.attrsets.mapAttrsToList
  #   (settings: settings.monitors )
  #   facts.network.hosts
  # ;
  monitor-to-exporter-name = monitor: {}.${monitor} or monitor;
  monitor-to-exporter = monitor: {
    ${monitor-to-exporter-name monitor} = {
      enable = true;
      openFirewall = true;
    } // ({
      node = let
        # Default port, but stated explicitly for clarity.
        port = 9100;
      in {
        inherit port;
        enabledCollectors = [
          "logind"
          "systemd"
        ];
        disabledCollectors = [
          "textfile"
        ];
        # openFirewall = true;
        # TODO: Always use long arguments if possible, or document that they
        # don't exist for the involved tool.  This was yanked directly from the
        # docs, which could possibly use expansion.
        # firewallFilter = "-i br0 -p tcp -m tcp --dport ${port}";
      };
    }.${monitor} or {});
  };
in {
  services.prometheus.exporters = lib.lists.fold (monitor: acc:
    acc // (monitor-to-exporter monitor)
  ) {} facts.network.hosts.${host-id}.monitors;
}
