################################################################################
# Dex handles an OIDC connections for LDAP.
################################################################################
{ config, host-id, lib-custom, ... }: let
  ldap-password-field = "ldap_bind_password";
  home-assistant-client-secret-field = "home_assistant_client_secret";
  dex-port = 5556;
  group = "dex-oidc";
in {
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
  } // (
    config.lib.ldap.ldap-password
      "dex-oidc"
      "${host-id}-dex-oidc-service"
  );
  users.groups.${group} = {};
  imports = [
    (import ../nixos-modules/https.nix {
      server-port = 5556;
      inherit host-id;
      fqdn = "dex.proton";
    })
  ];
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
            "https://home-assistant.proton/auth/external/callback"
          ];
          secret = "$''${home-assistant-client-secret-field}";
          name = "Home Assistant";
          trustedPeers = [];
        }
      ];
      issuer = "https://dex.proton";
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
            host = "nickel.proton:636";
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
    http."https://dex.proton" = {
      status = 200;
      timeout = 5000;
    };
    # Check that the OIDC discovery endpoint works.  This is the standard
    # endpoint that OIDC clients use to discover provider configuration.
    http."https://dex.proton/.well-known/openid-configuration" = {
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
      ip = [ "0.0.0.0" ];
    };
    # Check that the dex service is running.
    service.dex = {
      enabled = true;
      running = true;
    };
  };
}
