################################################################################
# A Prometheus client aggregates metrics to be reported to the Prometheus
# server.  Whether the client pushes this data or the server polls for it is
# both something configurable and not something I know the default of.
################################################################################
{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) pipe;
  inherit (lib.attrsets) filterAttrs mapAttrsToList;
  inherit (lib.lists) fold;
  monitor-to-exporter = monitor: {
    ${pkgs.lib.custom.monitor-to-exporter-name monitor} = {
      enable = true;
      openFirewall = true;
    }
    // (
      {
        node =
          let
          in
          {
            enabledCollectors = [
              "logind"
              "systemd"
            ];
            # TODO: Always use long arguments if possible, or document that they
            # don't exist for the involved tool.  This was yanked directly from the
            # docs, which could possibly use expansion.
            # firewallFilter = "-i br0 -p tcp -m tcp --dport ${port}";
          };
        # Query dnsmasq directly on port 5353 instead of the default port 53,
        # which is Blocky.  Without this, the exporter's CHAOS class queries
        # (.bind. domains) get forwarded by Blocky to upstream resolvers that
        # refuse them.
        dnsmasq = {
          dnsmasqListenAddress = "localhost:5353";
        };
        # The blackbox exporter is too simplistic for multi-tenant HTTPS services.
        # It does only one check.  So we leave it out and favor Gatus instead.  It
        # might make sense to
      }
      .${monitor} or { }
    );
  };

  # Filter out monitors that are handled by their own modules instead of via
  # services.prometheus.exporters.  For example, "goss" is handled by
  # goss-exporter.nix.
  exporterMonitors = builtins.filter (
    m:
    !builtins.elem m [
      "goss"
      "garage-queue-server"
      "garage-queue-worker"
    ]
  ) config.networking.monitors;

in
{
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
    (fold (monitor: acc: acc // (monitor-to-exporter monitor)) { })
  ];
}
