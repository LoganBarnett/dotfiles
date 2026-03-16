################################################################################
# Activates the ldap-server module and populates it from network facts.
#
# Generates a desired-state JSON file from facts.network.{users,groups} and
# runs ldap-reconciler as a systemd oneshot service on a timer to keep the
# live directory in sync.
#
# Separating this from ldap-server.nix follows the module/config split: the
# module declares what OpenLDAP can do; this config decides what it actually
# holds.
################################################################################
{
  config,
  facts,
  flake-inputs,
  lib,
  pkgs,
  system,
  ...
}:
let
  base-dn = "dc=proton,dc=org";

  # Only person and service accounts belong in LDAP.  The oidc-client type is
  # an OIDC client entry managed elsewhere and must not appear as an LDAP user.
  ldap-users = lib.filterAttrs (
    _: user: user.type == "person" || user.type == "service"
  ) facts.network.users;

  user-dn = username: "uid=${username},ou=users,${base-dn}";
  group-dn = name: "cn=${name},ou=groups,${base-dn}";

  # The systemd unit name determines the credential directory path.
  credential-path =
    name: "/run/credentials/ldap-reconciler.service/${name}-ldap-password-hashed";

  user-entry =
    username: user:
    lib.nameValuePair (user-dn username) (
      {
        objectClass = [
          "inetOrgPerson"
          "person"
          "top"
        ];
        cn = user.full-name;
        uid = username;
        sn = username;
        mail = user.email;
        # Unmanaged: set once on creation, then left to the user to change.
        # This preserves password changes across reconciliation runs.
        userPassword = {
          managed = false;
          initialPath = credential-path username;
        };
      }
      # Omit description when empty — LDAP DirectoryString requires >= 1 char.
      // lib.optionalAttrs (user.description != "") {
        description = user.description;
      }
    );

  group-entry =
    name: group:
    lib.nameValuePair (group-dn name) (
      {
        objectClass = [
          "groupOfNames"
          "top"
        ];
        cn = name;
        ou = name;
        # Managed: group membership stays authoritative from facts.
        member = map user-dn group.members;
      }
      // lib.optionalAttrs (group.description != "") {
        description = group.description;
      }
    );

  desired-state = {
    baseDn = base-dn;
    entries = {
      "${base-dn}" = {
        objectClass = [
          "domain"
          "top"
        ];
        dc = "proton";
      };
      "ou=users,${base-dn}" = {
        objectClass = [
          "organizationalUnit"
          "top"
        ];
        ou = "users";
        description = "Users in the proton network.";
      };
      "ou=groups,${base-dn}" = {
        objectClass = [
          "organizationalUnit"
          "top"
        ];
        ou = "groups";
        description = "Groups in the proton network.";
      };
    }
    // builtins.listToAttrs (lib.attrsets.mapAttrsToList user-entry ldap-users)
    // builtins.listToAttrs (
      lib.attrsets.mapAttrsToList group-entry facts.network.groups
    );
  };

  desired-state-file = pkgs.writeText "ldap-desired-state.json" (
    builtins.toJSON desired-state
  );

  ldap-reconciler-pkg = flake-inputs.ldap-reconciler.packages.${system}.default;

  # Credential name -> agenix secret path, one per LDAP user.
  user-credentials = lib.attrsets.mapAttrsToList (
    name: _:
    "${name}-ldap-password-hashed"
    + ":${config.age.secrets."${name}-ldap-password-hashed".path}"
  ) ldap-users;

in
{
  services.ldap-server.enable = true;

  # Declare hashed password secrets for every LDAP user.  The plaintext
  # variants are declared by facts-secrets.nix and consumed by individual
  # service configs; only the hashed form is needed here.
  age.secrets = config.lib.ldap.ldap-passwords "root" ldap-users;

  systemd.services.ldap-reconciler = {
    description = "LDAP desired-state reconciler";
    # Run after openldap is up so the socket is ready.
    after = [ "openldap.service" ];
    wants = [
      "openldap.service"
      "run-agenix.d.mount"
    ];
    serviceConfig = {
      Type = "oneshot";
      # Read the admin bind password from the credential, strip any trailing
      # newline, and pass it directly to the reconciler.
      ExecStart = pkgs.writeShellScript "ldap-reconciler-run" ''
        ${ldap-reconciler-pkg}/bin/ldap-reconciler \
          --ldap-url "ldaps://ldap.proton" \
          --ldap-bind-dn "cn=admin,${base-dn}" \
          --ldap-password "$(tr -d '\n' < /run/credentials/ldap-reconciler.service/ldap-root-pass)" \
          --state-file "${desired-state-file}"
      '';
      LoadCredential = [
        "ldap-root-pass:${config.age.secrets.ldap-root-pass.path}"
      ]
      ++ user-credentials;
      DynamicUser = true;
      PrivateTmp = true;
      ProtectSystem = "strict";
      ProtectHome = true;
    };
  };

  systemd.timers.ldap-reconciler = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      # Allow openldap to settle on boot before the first run.
      OnBootSec = "1min";
      OnUnitActiveSec = "1h";
    };
  };
}
