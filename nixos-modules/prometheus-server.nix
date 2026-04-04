################################################################################
# Provides a Prometheus server.
#
# Prometheus aggregates information across hosts via "exporters" that provide
# data sources for it.  The server is a central place where this data is
# collected and presented.
#
# Additional packages of interest include promtool - a CLI for Prometheus.
#
# See https://wiki.nixos.org/wiki/Prometheus for a writeup on how to get started
# with Prometheus in NixOS.
#
# ./prometheus-client.nix covers needs of the clients to run "exporters", which
# are what aggregate and offer data that the server can poll.  This might seem
# like the client/server nomenclature is backwards as I am using it.  Maybe it
# is, but this server file is meant to represent the location where prometheus
# aggregates and stores all of its exporters across all hosts.
#
# Configuring this module:
#
# The server must know about what exporters are available.  The convention used
# in this module is to search for amongst the facts metadata to determine what
# clients are out there and what exporters they use.
#
# While there may be many Prometheus exporters line up 1:1 with the monitor
# names used in the facts (./facts.nix), they aren't necessarily going to match
# fully - this is to keep the nomenclature more about monitoring and less
# specifically about Prometheus.  The facts should specify the intent and this
# module translates that to Prometheus configuration.  Also we might have a
# monitor differ in name but share the same exporter.
################################################################################
{
  config,
  facts,
  host-id,
  lib,
  nodes,
  pkgs,
  ...
}:
let
  inherit (lib) pipe;
  inherit (lib.attrsets) filterAttrs mapAttrsToList;
  inherit (lib.lists) fold;
in
{
  imports = [ ];
  networking.dnsAliases = [ "prometheus" ];
  services.https.fqdns."prometheus.${facts.network.domain}" = {
    internalPort = config.services.prometheus.port;
  };
  services.prometheus = {
    enable = true;
    retentionTime = "1y";
    extraFlags = [ "--web.enable-admin-api" ];
    # "1m" is another valid value.
    globalConfig.scrape_interval = "10s";
    scrapeConfigs =
      let
        # Resolve monitors for a single hostname from three sources:
        #  1. This host reads its own config directly to avoid infinite
        #     recursion through nodes.${host-id}.
        #  2. Other Nix-managed hosts expose monitors via their module.
        #  3. Non-Nix hosts fall back to the facts monitors field verbatim.
        host-monitors =
          hostname:
          if hostname == host-id then
            config.networking.monitors
          else if builtins.hasAttr hostname nodes then
            (nodes.${hostname}.config.networking.monitors or [ ])
          else
            (facts.network.hosts.${hostname}.monitors or [ ]);
        monitors = lib.pipe facts.network.hosts [
          (lib.attrsets.mapAttrsToList (host: _: host-monitors host))
          lib.lists.flatten
          lib.lists.unique
        ];
        host-targets =
          monitor:
          lib.pipe facts.network.hosts [
            (lib.attrsets.filterAttrs (
              host: settings:
              (settings.controlledHost or false)
              && !(settings.roaming or false)
              && (settings.expectedOnline or true)
              && (lib.lists.any (m: m == monitor) (host-monitors host))
            ))
            (lib.attrsets.mapAttrsToList (
              host: settings:
              "${host}:${
                toString (
                  # Goss is handled by its own module, not via
                  # services.prometheus.exporters, so use a hardcoded port.
                  if monitor == "goss" then
                    8080
                  else
                    nodes.${host}.config.services.prometheus.exporters.${pkgs.lib.custom.monitor-to-exporter-name monitor}.port
                )
              }"
            ))
          ];
      in
      lib.lists.map (
        monitor:
        (
          {
            job_name = monitor;
            static_configs = [
              {
                targets = host-targets monitor;
              }
            ];
          }
          // (
            {
              node = {
                static_configs = [
                  {
                    targets = host-targets monitor;
                  }
                ];
              };
              blackbox-ping = {
                # metrics_path = "/probe/blackbox-ping";
                # scrape_interval = "10s";
                params = {
                  modules = [ "icmp" ];
                };
                static_configs = [
                  {
                    targets = host-targets monitor;
                  }
                ];
                # relabel_configs = [
                #   {
                #     source_labels = [ "target" ];
                #     target_label = "__param_target";
                #   }
                # ];
              };
              systemd = {
                # Reduce frequency for systemd because it can cause a dbus clog.  It
                # doesn't need to be that fast anyways.
                scrape_interval = "60s";
              };
              wireguard = {
                static_configs = [
                  {
                    targets = host-targets monitor;
                  }
                ];
              };
              goss = {
                metrics_path = "/healthz";
                scrape_interval = "15s";
              };
            }
            .${monitor} or { }
          )
        )
      ) monitors;
  };
  # Goss health checks for Prometheus.
  services.goss.checks = {
    # Check that the HTTPS endpoint is responding.
    http."https://prometheus.${facts.network.domain}/" = {
      status = 200;
      timeout = 5000;
    };
    # Check that the Prometheus API is responding.
    http."https://prometheus.${facts.network.domain}/api/v1/status/config" = {
      status = 200;
      timeout = 3000;
    };
    # Check that the internal prometheus port is listening.  Prometheus binds
    # to :: (IPv6 any) which also accepts IPv4 connections via IPv4-mapped
    # IPv6 addresses.
    port."tcp6:${toString config.services.prometheus.port}" = {
      listening = true;
    };
    # Check that HTTPS port is listening (handled by reverse proxy).
    port."tcp:443" = {
      listening = true;
    };
    # Confirm the reverse proxy is reachable from all interfaces, not
    # only on one specific address.
    command."tcp:443-wildcard-binding" = pkgs.lib.custom.gossWildcardPortCheck 443;
    # Check that the prometheus service is running.
    service.prometheus = {
      enabled = true;
      running = true;
    };
  };
}
