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
{ config, host-id, lib, pkgs, ... }: let
  service-user-prefix = host-id;
  # How these get wired up is tricky.  We need to be able to delete them as
  # Grafana starts up, or it won't pick up changes.  So we need to safely emit
  # them somewhere where they can be "deleted" (removed symlinks).  We also want
  # changes to dashboards to issue a restart.  This is really hard to do in Nix,
  # since the settings are not desired to be in any of the provisioning
  # dashboard settings.  The evaluation means a content change isn't a name
  # change and thus it's the same as before - no service restart.  So instead we
  # use some systemd trickery to pull it off.
  #
  # Names must be valid POSIX file names.
  dashboards = {
    system-monitoring = {
      id = null;
      uid = "system-monitoring";
      schemaVersion = 16;
      title = "System Monitoring";
      panels = [
        {
          title = "CPU Usage %";
          type = "timeseries";
          datasource = "Prometheus";
          targets = [
            {
              expr = "(1 - avg by (instance) (rate(node_cpu_seconds_total{mode=\"idle\"}[5m]))) * 100";
              legendFormat = "{{instance}}";
              format = "time_series";
            }
          ];
          gridPos = { h = 9; w = 24; x = 0; y = 0; };
        }
        {
          title = "Memory Usage %";
          type = "timeseries";
          datasource = "Prometheus";
          targets = [
            {
              expr = "(1 - (node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes)) * 100";
              legendFormat = "{{instance}}";
              format = "time_series";
            }
          ];
          gridPos = { h = 9; w = 24; x = 0; y = 9; };
        }
        {
          title = "Disk Usage %";
          type = "timeseries";
          datasource = "Prometheus";
          targets = [
            {
              expr = "(1 - (node_filesystem_avail_bytes{fstype!=\"tmpfs\",fstype!=\"devtmpfs\"} / node_filesystem_size_bytes{fstype!=\"tmpfs\",fstype!=\"devtmpfs\"})) * 100";
              legendFormat = "{{instance}} - {{mountpoint}}";
              format = "time_series";
            }
          ];
          gridPos = { h = 9; w = 24; x = 0; y = 18; };
        }
        {
          title = "Network Traffic (TX + RX)";
          type = "timeseries";
          datasource = "Prometheus";
          targets = [
            {
              expr = ''
              sum by (instance) (
                rate(node_network_receive_bytes_total{device!~"lo|docker.*|veth.*"}[5m])
              )
            '';
              legendFormat = "{{instance}} RX";
              format = "time_series";
            }
            {
              expr = ''
              sum by (instance) (
                rate(node_network_transmit_bytes_total{device!~"lo|docker.*|veth.*"}[5m])
              )
            '';
              legendFormat = "{{instance}} TX";
              format = "time_series";
            }
          ];
          gridPos = { h = 9; w = 24; x = 0; y = 27; };
          fieldConfig = {
            defaults = {
              # This tells Grafana to auto-format bytes/sec.
              unit = "Bps";
              # This gives us some rounding.  Allows the auto formatting to
              # actually do its job.
              decimals = 3;
            };
            overrides = [];
          };
        }
        {
          title = "Load Average (1 min)";
          type = "timeseries";
          datasource = "Prometheus";
          targets = [
            {
              expr = "node_load1";
              legendFormat = "{{instance}}";
              format = "time_series";
            }
          ];
          gridPos = { h = 9; w = 24; x = 0; y = 36; };
        }
      ];
    };
    uptime = {
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
    };
  };
  # TODO: See comment in clean-grafana-dashboards about this path.  Ah but
  # there's this magic "static" subdirectory things really get moved to.
  dashboardPaths = lib.attrsets.mapAttrsToList (name: value:
    "/etc/static/grafana/dashboards/${name}.json"
  ) dashboards;
in {
  age.secrets = if config.services.grafana.enable then (
    config.lib.ldap.ldap-password
      "grafana"
      "${service-user-prefix}-grafana-service"
  ) else {};
  environment.etc = lib.attrsets.mapAttrs' (name: value: {
    name = "grafana/dashboards/${name}.json";
    value = { text = builtins.toJSON value; };
  }) dashboards;
  services.grafana = {
    enable = true;
    declarativePlugins = with pkgs.grafanaPlugins; [];
    provision = {
      enable = true;
      dashboards.settings.providers = lib.attrsets.mapAttrsToList (name: value: {
        inherit name;
        folder = "Provisioned";
        file = "/etc/grafana/dashboards/${name}.json";
        options.path = "/etc/grafana/dashboards";
        # type = "file";
        editable = false;
        disableDeletion = false;
      }) dashboards;
      datasources.settings.datasources = [
        {
          name = "Prometheus";
          type = "prometheus";
          url = "https://nickel.proton";
          access = "proxy";
          isDefault = true;
        }
      ];
    };
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
  };
  systemd.services.grafana = {
    wants = [ "run-agenix.d.mount" ];
    # Use a handy trick seen in
    # https://blog.withsam.org/blog/nixos-path-restart-trigger/ to promote
    # restarts.  Spoiler alert:  `restartTriggers` doesn't work seamlessly, but
    # if we serialize and hash our stuff, it's cool.
    restartTriggers =  [
      # Look at what you made me do.
      (builtins.hashString "sha256" (builtins.toJSON dashboards))
    ];
  };
  # The dashboards you see above are not truly declarative - they are purely
  # additive.  Some changes will be affected automatically, but others will not.
  # Surprise on which ones!  Nah, just delete them all and rebuild them.  Nix is
  # your master now.
  systemd.services.clean-grafana-dashboards = {
    description = "Clean out Grafana dashboards before startup";
    before = [ "grafana.service" ];
    wantedBy = [ "grafana.service" ];
    requiredBy = [ "grafana.service" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = let
        # I feel like this should be the value, but I'm doing things a little
        # too manually.  Let's see if we can use this later.
        # dashboardsDir = config.services.grafana.settings.paths.provisioning;
        dashboardsDir = "/etc/grafana/dashboards";
      in [
        # Safely wipe only .json files.
        "/run/current-system/sw/bin/find ${dashboardsDir} -type f -name '*.json' -delete"
      ];
      User = "grafana";
      Group = "grafana";
    };
  };
  networking.firewall.allowedTCPPorts = [ 3000 ];
  networking.firewall.allowedUDPPorts = [ 3000 ];
}
