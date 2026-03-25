################################################################################
# Activates the ldap-server module and populates it from auth.ldap.* options
# collected across all hosts in the cluster.
#
# Each service host declares its LDAP users and groups inline via the
# auth.ldap.{users,groups} options (provided by nixos-modules/ldap-auth.nix).
# This config aggregates those declarations from specialArgs.nodes and feeds
# them to the ldap-reconciler, which keeps the live directory in sync.
#
# ldap-auth-facts.nix handles the person accounts (sourced from facts.nix).
# Service accounts come from each service's own config via nodes.
################################################################################
{
  config,
  facts,
  flake-inputs,
  lib,
  nodes,
  pkgs,
  system,
  ...
}:
let
  base-dn = "dc=proton,dc=org";

  # Collect LDAP users from every host.  Darwin and container hosts that do
  # not import ldap-auth.nix contribute an empty attrset and are skipped.
  all-ldap-users = lib.foldlAttrs (
    acc: _: hostCfg:
    acc // (lib.attrByPath [ "config" "auth" "ldap" "users" ] { } hostCfg)
  ) { } nodes;

  # Collect LDAP groups from every host, merging member lists so each group
  # accumulates members from all contributing hosts.
  all-ldap-groups = lib.foldlAttrs (
    acc: _: hostCfg:
    lib.recursiveUpdate acc (
      lib.attrByPath [ "config" "auth" "ldap" "groups" ] { } hostCfg
    )
  ) { } nodes;

  # Users from other hosts whose secrets are not yet in config.age.secrets.
  # Silicon's own users are already emitted by ldap-auth.nix; only foreigners
  # need to be declared here to make them available as LoadCredential paths.
  foreign-ldap-users = lib.filterAttrs (
    name: _: !(lib.hasAttr name config.auth.ldap.users)
  ) all-ldap-users;

  user-dn = username: "uid=${username},ou=users,${base-dn}";
  group-dn = name: "cn=${name},ou=groups,${base-dn}";

  # The systemd unit name determines the credential directory path.
  credential-path =
    name: "/run/credentials/ldap-reconciler.service/${name}-ldap-password-hashed";

  user-entry =
    username: ucfg:
    lib.nameValuePair (user-dn username) (
      {
        objectClass = [
          "inetOrgPerson"
          "person"
          "top"
        ];
        cn = ucfg.fullName;
        uid = username;
        sn = username;
        mail = ucfg.email;
        # managed=true: reconciler always syncs from `path` (service accounts).
        # managed=false: reconciler sets the password only on initial creation
        # from `initialPath` (human accounts whose passwords evolve over time).
        userPassword =
          if ucfg.managed then
            {
              managed = true;
              path = credential-path username;
            }
          else
            {
              managed = false;
              initialPath = credential-path username;
            };
      }
      # Omit description when empty — LDAP DirectoryString requires >= 1 char.
      // lib.optionalAttrs (ucfg.description != "") {
        description = ucfg.description;
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
        # Managed: group membership stays authoritative from declared config.
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
    // builtins.listToAttrs (lib.mapAttrsToList user-entry all-ldap-users)
    // builtins.listToAttrs (lib.mapAttrsToList group-entry all-ldap-groups);
  };

  desired-state-file = pkgs.writeText "ldap-desired-state.json" (
    builtins.toJSON desired-state
  );

  ldap-reconciler-pkg = flake-inputs.ldap-reconciler.packages.${system}.default;

  # Credential name -> agenix secret path, one per LDAP user across all hosts.
  user-credentials = lib.mapAttrsToList (
    name: _:
    "${name}-ldap-password-hashed"
    + ":${config.age.secrets."${name}-ldap-password-hashed".path}"
  ) all-ldap-users;

in
{
  imports = [ ./ldap-auth-facts.nix ];

  services.ldap-server.enable = true;

  # Emit plaintext and hashed password secrets for users declared on foreign
  # hosts.  Those hosts' ldap-auth.nix already declared them with shared = true;
  # this gives silicon the rekeyed copies it needs to pass to the reconciler
  # via LoadCredential.  Silicon's own users are covered by ldap-auth.nix.
  age.secrets = lib.mkMerge (
    lib.mapAttrsToList (
      name: ucfg:
      lib.optionalAttrs ucfg.enable {
        "${name}-ldap-password" = {
          generator.script = "passphrase";
          group = "root";
          shared = true;
          rekeyFile = ../secrets/${name}-ldap-password.age;
          mode = "0440";
        };
        "${name}-ldap-password-hashed" = {
          group = "root";
          shared = true;
          generator = {
            script = "slapd-hashed";
            dependencies = [ config.age.secrets."${name}-ldap-password" ];
          };
          rekeyFile = ../secrets/${name}-ldap-password-hashed.age;
          mode = "0440";
        };
      }
    ) foreign-ldap-users
  );

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
          --ldap-url "ldaps://ldap.${facts.network.domain}" \
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
