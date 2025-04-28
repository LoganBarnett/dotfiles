################################################################################
# Provides a Grafana server.
#
# Grafana reads from a specialized metrics data source (such as Prometheus), and
# presents it in (hopefully) interesting and informative ways.  Typically this
# is in the form of some sort of graph - hence the name sounding like "graph".
#
# See https://wiki.nixos.org/wiki/Grafana for a writeup on how to get started
# with Grafana in NixOS.
#
#
################################################################################
{ config, host-id, pkgs, ... }: let
  service-user-prefix = host-id;
in {
  age.secrets = if config.services.grafana.enable then (
    config.lib.ldap.ldap-password
      "grafana"
      "${service-user-prefix}-grafana-service"
  ) else {};
  environment.etc."grafana/dashboards/uptime.json".text = builtins.toJSON {
    # dashboard = {
      id = null;
      uid = "uptime";
      schemaVersion = 16;
      title = "Uptime";
      panels = [
        {
          type = "timeseries";
          title = "Host Uptime";
          datasource = "Prometheus";
          targets = [
            {
              expr = "up";
              format = "time_series";
              legendFormat = "{{instance}}";
            }
          ];
          gridPos = {
            h = 9;
            w = 24;
            x = 0;
            y = 0;
          };
          options = {
            legend = {
              show = true;
              displayMode = "table";
              # displayMode = "list";
            };
          };
        }
      ];
    # };
  };
  services.grafana = {
    enable = true;
    declarativePlugins = with pkgs.grafanaPlugins; [];
    provision.dashboards.settings.providers = [
      {
        name = "Uptime Dashboard";
        editable = false;
        folder = "Provisioned";
        options.path = "/etc/grafana/dashboards";
      }
    ];
    provision.datasources.settings.datasources = [
      {
        name = "Prometheus";
        type = "prometheus";
        url = "https://nickel.proton";
        access = "proxy";
        isDefault = true;
      }
    ];
    settings = {
      "auth.ldap" = {
        enabled = true;
        config_file = (toString ((pkgs.formats.toml {}).generate "ldap.toml" {
          servers = [
            {
              host = "nickel.proton";
              port = 636;
              use_ssl = true;
              start_tls = false;
              tls_ciphers = [];
              min_tls_version = "";
              ssl_skip_verify = false;
              # This might not be needed, since it should probably use the
              # system trust.
              root_ca_cert = ../secrets/proton-ca.crt;
              bind_dn =
                "uid=${service-user-prefix}-grafana-service,ou=users,dc=proton,dc=org"
              ;
              bind_password = "$__file{${
                config
                  .age
                  .secrets
                  ."${service-user-prefix}-grafana-service-ldap-password"
                  .path
              }}";
              timeout = 10;
              search_filter = "(uid=%s)";
              search_base_dns = ["dc=proton,dc=org"];
              # Deliberately left out to disable.
              # group_search_filter = "";
              # Deliberately left out to disable.
              # group_search_filter_user_attribute = "";
              group_search_filter_base_dns = [
                "ou=groups,dc=proton,dc=org"
              ];
              attributes = {
                email = "mail";
                member_of = "memberOf";
                username = "uid";
              };
              group_mappings = [
                {
                  group_dn = "cn=grafana-admins,ou=groups,dc=proton,dc=org";
                  org_role = "Admin";
                  grafana_admin = true;
                }
                {
                  group_dn = "cn=grafana-viewers,ou=groups,dc=proton,dc=org";
                  org_role = "Viewer";
                }
              ];
            }
          ];
          # Undocumented?  But talked about here:
          # https://stackoverflow.com/a/47594344
          # The documented approach of setting `filters = ldap:debug` only adds
          # a single, unhelpful statement for authentication issues.
          # It doesn't change anything though.  I believe this is from an older
          # version.
          # verbose_logging = true;
        }))
        ;
        # Allow sign-up should be `true` (default) to allow Grafana to create
        # users on successful LDAP authentication.  If set to `false` only
        # already existing Grafana users will be able to login.
        allow_sign_up = true;
        # Prevent synchronizing ldap users organization roles.
        # skip_org_role_sync = true;
        skip_org_role_sync = false;
      };
      log = {
        filters = "ldap:debug";
      };
    };
    # TODO: Connect to Prometheus.
    # TODO: Create a network connectivity dashboard.
  };
  systemd.services.grafana = {
    wants = [ "run-agenix.d.mount" ];
  };
  # TODO: Remove once this is properly moved to a virtual host.
  networking.firewall.allowedTCPPorts = [ 3000 ];
  networking.firewall.allowedUDPPorts = [ 3000 ];
}
