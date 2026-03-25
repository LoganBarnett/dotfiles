################################################################################
# TOMBSTONE — Dex is no longer active.
#
# Dex was our original OIDC broker, using LDAP as its identity backend.  It
# served Home Assistant as its only active client.  We migrated to Authelia
# (nixos-configs/authelia.nix on silicon) because Authelia provides:
#
#   - Password reset flows (Dex has none)
#   - A foundation for MFA if we ever want it
#   - A richer access-control model (per-domain policies, group rules)
#
# Home Assistant was decommissioned before the migration completed, so Dex
# never had a live client to migrate.  The import in hosts/copper.nix was
# already commented out.  This file is kept for historical reference.
################################################################################
{
  config,
  facts,
  host-id,
  lib-custom,
  pkgs,
  ...
}:
let
  ldap-password-field = "ldap_bind_password";
  home-assistant-client-secret-field = "home_assistant_client_secret";
  dex-port = 5556;
  group = "dex-oidc";
in
{
  auth.ldap.users."${host-id}-dex-oidc-service" = {
    email = "${host-id}-dex-oidc-service@proton";
    fullName = "${host-id}-dex-oidc-service";
    description = "Dex-OIDC service account on ${host-id}.";
    group = group;
  };
  age.secrets = {
    dex-oidc-ldap-environment-file = {
      generator = {
        script = "environment-variable";
        dependencies = [
          config.age.secrets."${host-id}-dex-oidc-service-ldap-password"
        ];
      };
      settings.field = ldap-password-field;
      inherit group;
      rekeyFile = ../secrets/dex-oidc-ldap-environment-file.age;
    };
    home-assistant-client-secret-environment-file = {
      generator = {
        script = "environment-variable";
        dependencies = [
          config.age.secrets."home-assistant-client-secret"
        ];
      };
      settings.field = home-assistant-client-secret-field;
      inherit group;
      rekeyFile = ../secrets/home-assistant-client-secret-environment-file.age;
    };
    dex-oidc-environment-file = {
      generator = {
        script = "environment-file";
        dependencies = [
          config.age.secrets.home-assistant-client-secret-environment-file
          config.age.secrets.dex-oidc-ldap-environment-file
        ];
      };
      rekeyFile = ../secrets/dex-oidc-environment-file.age;
    };
  };
  users.groups.${group} = { };
  imports = [ ];
  services.https.fqdns."dex.${facts.network.domain}" = {
    internalPort = dex-port;
  };
  # The dex NixOS module uses DynamicUser = true and thus requires a little more
  # plumbing to work.
  systemd.services.dex = {
    serviceConfig = {
      SupplementaryGroups = [ group ];
    };
  };
  services.dex = {
    enable = true;
    environmentFile = config.age.secrets.dex-oidc-ldap-environment-file.path;
    settings = {
      clients = [
        {
          id = "home-assistant";
          redirectURIs = [
            "https://home-assistant.${facts.network.domain}/auth/external/callback"
          ];
          secret = "$''${home-assistant-client-secret-field}";
          name = "Home Assistant";
          trustedPeers = [ ];
        }
      ];
      issuer = "https://dex.${facts.network.domain}";
      storage.type = "memory";
      web.http = "0.0.0.0:${toString dex-port}";
      # storage = {
      #   type = "postgres";
      #   config.host = "/var/run/postgres";
      # };
      connectors = [
        {
          type = "ldap";
          id = "ldap";
          name = "ldap";
          config = {
            host = "ldap.${facts.network.domain}:636";
            insecureNoSSL = false;
            bindDN = "uid=dex-oidc,ou=services,dc=proton,dc=org";
            bindPW = "$''${ldap-password-field}";
            userSearch = {
              baseDN = "dc=proton,dc=org";
              filter = "(objectClass=inetOrgPerson)";
              username = "username";
              idAttr = "uid";
              emailAttr = "mail";
              nameAttr = "cn";
            };
          };
        }
      ];
    };
  };
  # Goss health checks for Dex.
  services.goss.checks = {
    # Check that the HTTPS endpoint is responding.
    http."https://dex.${facts.network.domain}" = {
      status = 200;
      timeout = 5000;
    };
    # Check that the OIDC discovery endpoint works.  This is the standard
    # endpoint that OIDC clients use to discover provider configuration.
    http."https://dex.${facts.network.domain}/.well-known/openid-configuration" = {
      status = 200;
      timeout = 3000;
      headers = [ "Content-Type: application/json" ];
    };
    # Check that the internal dex port is listening.
    port."tcp:${toString dex-port}" = {
      listening = true;
      ip = [ "0.0.0.0" ];
    };
    # Check that HTTPS port is listening (handled by reverse proxy).
    port."tcp:443" = {
      listening = true;
    };
    # Confirm the reverse proxy is reachable from all interfaces, not
    # only on one specific address.
    command."tcp:443-wildcard-binding" = pkgs.lib.custom.gossWildcardPortCheck 443;
    # Check that the dex service is running.
    service.dex = {
      enabled = true;
      running = true;
    };
  };
}
