################################################################################
# A Prometheus client aggregates metrics to be reported to the Prometheus
# server.  Whether the client pushes this data or the server polls for it is
# both something configurable and not something I know the default of.
################################################################################
{ config, lib, facts, host-id, pkgs, ... }: let
  inherit (lib) pipe;
  inherit (lib.attrsets) filterAttrs mapAttrsToList;
  inherit (lib.lists) fold;
  hostFacts = facts.network.hosts.${host-id};
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
      in {
        enabledCollectors = [
          "logind"
          "systemd"
        ];
        disabledCollectors = [
          "textfile"
        ];
        # TODO: Always use long arguments if possible, or document that they
        # don't exist for the involved tool.  This was yanked directly from the
        # docs, which could possibly use expansion.
        # firewallFilter = "-i br0 -p tcp -m tcp --dport ${port}";
      };
      # The blackbox exporter is too simplistic for multi-tenant HTTPS services.
      # It does only one check.  So we leave it out and favor Gatus instead.  It
      # might make sense to
    }.${monitor} or {});
  };

  # Filter out monitors that are handled by their own modules instead of via
  # services.prometheus.exporters.  For example, "goss" is handled by
  # goss-exporter.nix.
  exporterMonitors = builtins.filter
    (m: ! builtins.elem m ["goss"])
    hostFacts.monitors;

in {
  # TODO: Move this to a gatus specific config which lifts from
  # facts.network.services.  Select a host that runs gatus (probably argon but
  # look).
  # services.gatus = let
  #   enable = (builtins.length hostFacts.monitors) > 0;
  # in {
  #   inherit enable;
  #   openFirewall = enable;
  # };
  services.prometheus.exporters = pipe exporterMonitors [
    (fold (monitor: acc: acc // (monitor-to-exporter monitor)) {})
  ];
}
