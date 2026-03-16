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
{
  config,
  facts,
  host-id,
  lib,
  pkgs,
  ...
}:
let
  service-user-prefix = host-id;
  # Dashboard definitions live in facts so both this host and the kiosk host
  # can hash them independently for restart triggers.
  dashboards = facts.network.monitoring.grafanaDashboards { inherit lib facts; };
in
{
  imports = [
    (import ../nixos-modules/https.nix {
      server-port = config.services.grafana.settings.server.http_port;
      inherit host-id;
      fqdn = "grafana.proton";
    })
  ];
  environment.systemPackages = [
    # Include sqlite because it's what Grafana uses in the default NixOS setup.
    # This can allow us to chase down database issues if the API isn't
    # forthcoming to us.
    pkgs.sqlite
  ];
  environment.etc = lib.attrsets.mapAttrs' (name: value: {
    name = "grafana/dashboards/provisioned/${name}.json";
    value = {
      text = builtins.toJSON value;
    };
  }) dashboards;
  networking.firewall.allowedTCPPorts = [ 3000 ];
  networking.firewall.allowedUDPPorts = [ 3000 ];
  services.grafana = {
    enable = true;
    declarativePlugins = with pkgs.grafanaPlugins; [ ];
    provision = {
      enable = true;
      # A provider in this context is a dashboard provider.  It's not quite a
      # "folder" but it is close to that.
      dashboards.settings.providers = [
        {
          name = "provisioned-dashboards";
          options.path = "/etc/grafana/dashboards/provisioned";
          folder = "Provisioned";
          disableDeletion = false;
          updateIntervalSeconds = 60;
        }
      ];
      datasources.settings.datasources = [
        {
          name = "Prometheus";
          type = "prometheus";
          url = "https://prometheus.proton";
          access = "proxy";
          isDefault = true;
        }
      ];
    };
    settings = {
      # Enable anonymous access to the dashboards.  Everything is exposed
      # publicly via Prometheus exporters anyways.
      "auth.anonymous" = {
        enabled = true;
        # org_name = "Main Org.";
        org_role = "Viewer";
        hide_version = false;
        # device_limit = 1;
      };
      "auth.ldap" = {
        enabled = true;
        config_file = (
          toString (
            (pkgs.formats.toml { }).generate "ldap.toml" {
              servers = [
                {
                  host = "nickel.proton";
                  port = 636;
                  use_ssl = true;
                  start_tls = false;
                  tls_ciphers = [ ];
                  min_tls_version = "";
                  ssl_skip_verify = false;
                  # This might not be needed, since it should probably use the
                  # system trust.
                  root_ca_cert = ../secrets/proton-ca.crt;
                  bind_dn = "uid=${service-user-prefix}-grafana-service,ou=users,dc=proton,dc=org";
                  bind_password = "$__file{${
                    config.age.secrets."${service-user-prefix}-grafana-service-ldap-password".path
                  }}";
                  timeout = 10;
                  search_filter = "(uid=%s)";
                  search_base_dns = [ "dc=proton,dc=org" ];
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
            }
          )
        );
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
    restartTriggers = [
      # Look at what you made me do.
      (builtins.hashString "sha256" (builtins.toJSON dashboards))
    ];
  };
  # The dashboards you see above are not truly declarative - they are purely
  # additive.  Some changes will be affected automatically, but others will not.
  # Surprise on which ones!  Nah, just delete them all and rebuild them.  Nix is
  # your master now.
  #
  # TODO: Test this out now that we've fixed the issue with "duplicate
  # dashboards".
  systemd.services.clean-grafana-dashboards = {
    description = "Clean out Grafana dashboards before startup.";
    before = [ "grafana.service" ];
    wantedBy = [ "grafana.service" ];
    requiredBy = [ "grafana.service" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart =
        let
          # I feel like this should be the value, but I'm doing things a little
          # too manually.  Let's see if we can use this later.
          # dashboardsDir = config.services.grafana.settings.paths.provisioning;
          dashboardsDir = "/etc/grafana/dashboards";
        in
        [
          # Safely wipe only .json files.
          # TODO: Let's emit the JSON files with a prefix such as
          # "__nix-provisioned-${name}".  Then we can find `__nix-provisioned-*`
          # and only delete those, and leave dynamically created ones intact.
          # This might be a worthy submission to add.
          "/run/current-system/sw/bin/find ${dashboardsDir} -type f -name '*.json' -delete"
        ];
      User = "grafana";
      Group = "grafana";
    };
  };
  users.users.grafana.extraGroups = [ "openldap-${host-id}-grafana-service" ];
  users.groups."openldap-${host-id}-grafana-service" = { };
  # Goss health checks for Grafana.
  services.goss.checks = {
    # Check that the HTTPS endpoint is responding.
    http."https://grafana.proton/" = {
      status = 200;
      timeout = 5000;
    };
    # Check that the API health endpoint works.
    http."https://grafana.proton/api/health" = {
      status = 200;
      timeout = 3000;
    };
    # Check that the internal grafana port is listening.  Grafana listens on
    # loopback only by default.
    port."tcp:${toString config.services.grafana.settings.server.http_port}" = {
      listening = true;
      ip = [ "127.0.0.1" ];
    };
    # Check that HTTPS port is listening (handled by reverse proxy).
    port."tcp:443" = {
      listening = true;
      ip = [ "0.0.0.0" ];
    };
    # Check that the grafana service is running.
    service.grafana = {
      enabled = true;
      running = true;
    };
  };
}
