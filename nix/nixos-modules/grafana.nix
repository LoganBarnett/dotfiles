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
  services.grafana = {
    enable = true;
    declarativePlugins = with pkgs.grafanaPlugins; [];
    settings = {
      log = {
        filters = "ldap:debug";
      };
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
              group_search_filter = "(member=%s)";
              group_search_filter_user_attribute = "cn=%s";
              group_search_filter_base_dns = [
                "ou=grafana-admins"
                "ou=grafana-viewers"
              ];
              attributes = {
                member_of = "member";
                email = "mail";
                username = "uid";
              };
              group_mappigs = [
                {
                  group_dn = "cn=grafana-admins,dc=proton,dc=org";
                  org_role = "Admin";
                  grafana_admin = true;
                }
                {
                  group_dn = "cn=grafana-viewers,dc=proton,dc=org";
                  org_role = "Viewer";
                }
              ];
              verbose_logging = true;
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
