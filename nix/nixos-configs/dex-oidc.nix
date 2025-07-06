################################################################################
# Dex handles an OIDC connections for LDAP.
################################################################################
{ config, host-id, lib-custom, ... }: let
  ldap-password-field = "ldap_bind_password";
  dex-port = 5556;
  group = "dex-oidc";
in {
  age.secrets = {
    dex-oidc-ldap-environment-file = {
      generator = {
        script = "environment-file";
        dependencies = [
          config.age.secrets."${host-id}-dex-oidc-service-ldap-password"
        ];
      };
      settings.field = ldap-password-field;
      inherit group;
      mode = "0440";
      rekeyFile = ../secrets/dex-oidc-ldap-environment-file.age;
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
}
