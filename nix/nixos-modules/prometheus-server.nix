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
{ config, facts, host-id, lib, pkgs, ... }: {
  imports = [
    (import ../nixos-modules/https.nix {
      server-port = 9090;
      inherit host-id;
      fqdn = "prometheus.proton";
    })
  ];
  services.prometheus = {
    enable = true;
    # "1m" is another valid value.
    globalConfig.scrape_interval = "10s";
    scrapeConfigs = let
      monitors = lib.pipe facts.network.hosts [
        (lib.attrsets.mapAttrsToList (host: settings: settings.monitors))
        lib.lists.flatten
        lib.lists.unique
      ];
      host-targets = monitor: lib.pipe facts.network.hosts [
        (lib.attrsets.filterAttrs (host: settings:
          settings.controlledHost
          && (!settings.roaming or false)
          && (lib.lists.any (m: m == monitor) settings.monitors)
        ))
        (lib.attrsets.mapAttrsToList (host: settings:
          "${host}:${
            toString config
              .services
              .prometheus
              .exporters
              .${pkgs.lib.custom.monitor-to-exporter-name monitor}
              .port
          }"
        ))
      ]
      ;
    in lib.lists.map
      (monitor:
        ({
          job_name = monitor;
          static_configs = [
            {
              targets = host-targets monitor;
            }
          ];
        } // ({
          node = {
            static_configs = [{
              targets = host-targets monitor;
            }];
          };
          blackbox-ping = {
            # metrics_path = "/probe/blackbox-ping";
            # scrape_interval = "10s";
            params = {
              modules = [ "icmp" ];
            };
            static_configs = [{
              targets = host-targets monitor;
            }];
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
        }.${monitor} or { }))
      )
      monitors;
  };
}
