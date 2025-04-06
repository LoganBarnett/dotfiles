################################################################################
# A Prometheus client aggregates metrics to be reported to the Prometheus
# server.  Whether the client pushes this data or the server polls for it is
# both something configurable and not something I know the default of.
################################################################################
{ config, lib, facts, host-id, pkgs, ... }: let
  # monitors = lib.attrsets.mapAttrsToList
  #   (settings: settings.monitors )
  #   facts.network.hosts
  # ;
  monitor-to-exporter = monitor: {
    ${pkgs.lib.custom.monitor-to-exporter-name monitor} = {
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
      blackbox-ping = {
        configFile = pkgs.writeTextFile {
          name = "blackbox-ping.yaml";
          text = (builtins.toJSON {
            modules = {
              icmp = {
                prober = "icmp";
                timeout = "10s";
                icmp = {
                  preferred_ip_protocol = "ip4";
                };
              };
            };
          });
        };
      };
    }.${monitor} or {});
  };
in {
  services.prometheus.exporters = lib.lists.fold (monitor: acc:
    acc // (monitor-to-exporter monitor)
  ) {} facts.network.hosts.${host-id}.monitors;
}
