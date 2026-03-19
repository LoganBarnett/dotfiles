################################################################################
# Extends the upstream nixpkgs services.goss module with:
#
#  - services.goss.checks: declarative check contributions.  Individual
#    service configs set entries here; this module aggregates them into
#    services.goss.settings so the upstream module writes them to goss.yaml.
#
#  - services.goss.prometheusContentTypeFixProxy: optional nginx reverse proxy
#    that rewrites the Content-Type header from the non-standard value goss
#    ≤ 0.4.9 emits ("application/vnd.goss-prometheus") to the value Prometheus
#    3.x requires ("text/plain; version=0.0.4").  Enable this until nixpkgs
#    carries a goss release that includes the fix from
#    https://github.com/goss-org/goss/pull/1022.
#
# All behaviour is gated on services.goss.enable, which the upstream module
# defines.  This module adds no enable option of its own.
################################################################################
{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    types
    ;

  cfg = config.services.goss;
  proxyEnabled = cfg.prometheusContentTypeFixProxy.enable;
  proxyPort = cfg.prometheusContentTypeFixProxy.port;
  # Goss binds one port above the public-facing port when the proxy is in use
  # so nginx can sit in front; otherwise it binds the public port directly.
  gossPort = if proxyEnabled then proxyPort + 1 else proxyPort;
in
{
  options.services.goss = {
    checks = mkOption {
      type = types.submodule { freeformType = (pkgs.formats.yaml { }).type; };
      default = { };
      example = {
        http."https://git.example.com" = {
          status = 200;
          timeout = 5000;
        };
        port."tcp:22" = {
          listening = true;
        };
      };
      description = ''
        Goss health check definitions.  Individual service configurations
        contribute checks here; this module merges them into
        services.goss.settings so the upstream module writes them to goss.yaml.

        See the full check reference at
        <https://github.com/goss-org/goss/blob/master/docs/manual.md>.
      '';
    };

    prometheusContentTypeFixProxy = {
      enable = mkEnableOption ''
        nginx reverse proxy that rewrites the Content-Type header emitted by
        goss ≤ 0.4.9 to the value Prometheus 3.x requires.  Goss binds to
        port + 1 internally; nginx listens on port and rewrites the header on
        /healthz responses
      '';

      port = mkOption {
        type = types.port;
        default = 8080;
        description = ''
          Public-facing port.  When the proxy is enabled, nginx listens here
          and goss binds to port + 1.  When the proxy is disabled, goss binds
          here directly.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    # Merge declarative check contributions into the upstream settings attrset
    # so the upstream module writes them to goss.yaml.
    services.goss.settings = cfg.checks;

    # Run goss in Prometheus serve mode.  The verbose format option adds a
    # resource_id label to each metric so check names are visible in Prometheus
    # and Grafana.  The upstream module does not expose extraArgs, so we extend
    # ExecStart directly.
    services.goss.environment.GOSS_FMT = "prometheus";
    services.goss.environment.GOSS_LISTEN = ":${toString gossPort}";
    systemd.services.goss.serviceConfig.ExecStart = lib.mkForce ''
      ${cfg.package}/bin/goss serve --format-options verbose
    '';

    services.nginx = mkIf proxyEnabled {
      enable = true;
      virtualHosts."goss-proxy" = {
        listen = [
          {
            addr = "0.0.0.0";
            port = proxyPort;
          }
        ];
        locations."/healthz" = {
          proxyPass = "http://127.0.0.1:${toString gossPort}/healthz";
          extraConfig = ''
            proxy_hide_header Content-Type;
            add_header Content-Type "text/plain; version=0.0.4; charset=utf-8";
          '';
        };
      };
    };

    networking.firewall.allowedTCPPorts = [ proxyPort ];
  };
}
