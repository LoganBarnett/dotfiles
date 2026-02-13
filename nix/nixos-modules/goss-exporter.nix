################################################################################
# Provides goss health checking with Prometheus export.
#
# This module provides a `services.goss.checks` option that service
# configurations can use to declare their health checks.  When the "goss"
# monitor is enabled in facts, this module aggregates all checks into a unified
# goss.yaml and runs `goss serve` with Prometheus format output.
#
# Individual services should contribute checks via `services.goss.checks`.
# For example, in nixos-configs/gitea.nix:
#
#   services.goss.checks = {
#     http."https://git.proton" = {
#       status = 200;
#       timeout = 5000;
#     };
#     port."tcp:2222" = {
#       listening = true;
#     };
#   };
################################################################################
{ config, lib, pkgs, facts, host-id, ... }:
let
  inherit (lib) mkOption mkIf types mkMerge;

  hostFacts = facts.network.hosts.${host-id};
  gossEnabled = builtins.elem "goss" (hostFacts.monitors or []);

  # Aggregate all goss check contributions from various services.
  # The module system should merge these automatically, but we ensure
  # deep merging by using the final merged value.
  allChecks = config.services.goss.checks;

  # Generate the unified goss.yaml configuration file.
  gossConfigFile = pkgs.writeText "goss.yaml" (builtins.toJSON allChecks);

  gossPort = 8080;

in {
  options.services.goss = {
    checks = mkOption {
      type = types.attrsOf types.attrs;
      default = {};
      description = ''
        Goss health check configuration.  Services contribute their own checks
        here, which are aggregated into a single goss.yaml file.

        See https://github.com/goss-org/goss/blob/master/docs/manual.md for
        the full specification of available check types.

        Common check types:
        - http: HTTP endpoint checks (status codes, response body, headers)
        - port: TCP/UDP port listening checks
        - service: systemd service status checks
        - process: Process running checks
        - file: File existence, permissions, content checks
        - command: Command execution and output checks
        - dns: DNS resolution checks
        - mount: Filesystem mount checks
        - interface: Network interface checks
        - kernel-param: Kernel parameter checks
        - user: User existence and properties
        - group: Group existence and properties
        - package: Package installation checks
        - matching: Advanced pattern matching checks
      '';
      example = lib.literalExpression ''
        {
          http = {
            "https://git.proton" = {
              status = 200;
              timeout = 5000;
              body = [ "Gitea" ];
            };
            "https://git.proton/api/v1/version" = {
              status = 200;
              timeout = 3000;
              headers = [ "Content-Type: application/json" ];
            };
          };
          port = {
            "tcp:2222" = {
              listening = true;
              ip = [ "0.0.0.0" ];
            };
          };
          service = {
            gitea = {
              enabled = true;
              running = true;
            };
          };
        }
      '';
    };
  };

  config = mkIf gossEnabled {
    # Install goss package system-wide for debugging.
    environment.systemPackages = [ pkgs.goss ];

    # Run goss serve as a systemd service on internal port.
    systemd.services.goss-exporter = {
      description = "Goss Health Check Prometheus Exporter";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];

      serviceConfig = {
        Type = "simple";
        ExecStart = ''
          ${pkgs.goss}/bin/goss \
            --gossfile ${gossConfigFile} \
            serve \
            --format prometheus \
            --listen-addr :${toString (gossPort + 1)}
        '';
        Restart = "always";
        RestartSec = "10s";

        # Security hardening.
        DynamicUser = true;
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;

        # Grant network capabilities for port checking.  This allows goss to
        # see all listening ports on the system, which is necessary for
        # accurate port availability checks.
        AmbientCapabilities = [ "CAP_NET_ADMIN" "CAP_NET_RAW" ];
        CapabilityBoundingSet = [ "CAP_NET_ADMIN" "CAP_NET_RAW" ];

        # Allow network access for HTTP checks.
        PrivateNetwork = false;
      };
    };

    # Nginx reverse proxy to fix content-type header for Prometheus.
    # Goss uses "application/vnd.goss-prometheus" which Prometheus doesn't
    # recognize in newer versions.  This proxy changes it to the standard
    # Prometheus text format content type.
    services.nginx.enable = true;
    services.nginx.virtualHosts."goss-proxy" = {
      listen = [ { addr = "0.0.0.0"; port = gossPort; } ];
      locations."/healthz" = {
        proxyPass = "http://127.0.0.1:${toString (gossPort + 1)}/healthz";
        extraConfig = ''
          proxy_hide_header Content-Type;
          add_header Content-Type "text/plain; version=0.0.4; charset=utf-8";
        '';
      };
    };

    # Open firewall for Prometheus to scrape (nginx proxy port).
    networking.firewall.allowedTCPPorts = [ gossPort ];
  };
}
