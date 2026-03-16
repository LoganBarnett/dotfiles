################################################################################
# Prometheus exporter for Goss health checks on macOS.
#
# Builds on darwin-modules/goss.nix (which must also be imported).  Adds
# services.goss.prometheus options, configures goss to serve in prometheus
# format on an internal port, and fronts it with a local nginx daemon that
# rewrites the Content-Type header.
#
# goss ≤ 0.4.9 serves "application/vnd.goss-prometheus" instead of the
# "text/plain; version=0.0.4" that Prometheus 3.x requires.  The fix is merged
# upstream (https://github.com/goss-org/goss/pull/1022) but not yet released.
# Once nixpkgs carries a fixed release, remove the nginx daemon and let goss
# bind services.goss.prometheus.port directly.
################################################################################
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.goss;
  gossPort = cfg.prometheus.port;

  nginxConf = pkgs.writeText "goss-nginx.conf" ''
    pid /tmp/goss-nginx.pid;
    error_log /var/log/goss-nginx.log warn;

    events {
      worker_connections 16;
    }

    http {
      access_log off;
      client_body_temp_path /tmp;
      proxy_temp_path /tmp;

      server {
        listen ${toString gossPort};
        location /healthz {
          proxy_pass http://127.0.0.1:${toString (gossPort + 1)}/healthz;
          proxy_hide_header Content-Type;
          add_header Content-Type "text/plain; version=0.0.4; charset=utf-8";
        }
      }
    }
  '';

in
{
  options.services.goss.prometheus = {
    enable = lib.mkEnableOption "Goss Prometheus exporter";

    port = lib.mkOption {
      type = lib.types.port;
      default = 8080;
      description = ''
        Port on which the Prometheus exporter is exposed.  Goss itself binds to
        port + 1 internally; the nginx proxy in front rewrites the Content-Type
        header and listens on this port.
      '';
    };
  };

  config = lib.mkIf cfg.prometheus.enable {
    # Ensure the base goss module is active.
    services.goss.enable = lib.mkDefault true;

    # Drive goss into prometheus serve mode on the internal port.
    services.goss.environment = {
      GOSS_FMT = "prometheus";
      GOSS_LISTEN = ":${toString (gossPort + 1)}";
    };

    launchd.daemons.goss-nginx = {
      serviceConfig = {
        Label = "org.goss.nginx";
        ProgramArguments = [
          "${pkgs.nginx}/bin/nginx"
          "-c"
          "${nginxConf}"
          "-g"
          "daemon off;"
        ];
        RunAtLoad = true;
        KeepAlive = true;
        StandardErrorPath = "/var/log/goss-nginx.log";
      };
    };
  };
}
