################################################################################
# Keycloak is basically being used here an adapter to take an LDAP server and
# provide OIDC authentication.  It can do more than this though.
#
# Some systems don't support LDAP directly and instead have OIDC support.  In
# some cases OIDC might actually be preferable because it's single sign on.
################################################################################
{ config, host-id, ... }: {
  imports = [
    (import ../nixos-modules/https.nix {
      server-port = config.services.keycloak.port;
      inherit host-id;
      fqdn = "${host-id}.proton";
    })
  ];
  age.secrets.keycloak-database-password = {
    generator.script = "long-passphrase";
    group = "keycloak";
    mode = "0440";
  };
  services.keycloak = {
    enable = true;
    database.passwordFile = config.age.secrets.keycloak-database-password.path;
    settings = {
      db = "postgres";
      hostname = "keycloak.proton";
    };
    realmFiles = [
      (builtins.writeTextFile {
        name = "realm.json";
        text = builtins.toJSON {
          realm = "proton";
          enabled = true;
          id = "proton";
            "userFederationProviders": [
              {
                "providerName": "ldap",
                "priority": 0,
                "displayName": "My LDAP",
                "config": {
                  "editMode": ["READ_ONLY"],
                  "vendor": ["other"],
                  "usernameLDAPAttribute": ["uid"],
                  "rdnLDAPAttribute": ["uid"],
                  "uuidLDAPAttribute": ["entryUUID"],
                  "userObjectClasses": ["inetOrgPerson"],
                  "connectionUrl": ["ldap://ldap.proton.org"],
                  "usersDn": ["dc=proton,dc=org"],
                  "authType": ["simple"],
                  "bindDn": ["uid=keycloak,ou=services,dc=proton,dc=org"],
                  "bindCredential": ["secret-ldap-password"],
                  "pagination": ["true"],
                  "syncRegistrations": ["false"],
                  "importEnabled": ["true"]
                }
              }
            ]
};
      })
    ];
  };
}
