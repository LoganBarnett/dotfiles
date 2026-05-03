################################################################################
# Activates the ldap-server module and populates it from auth.ldap.* options
# collected across all hosts in the cluster, via the typed nix-hapi-ldap
# module.
#
# Each service host declares its LDAP users and groups inline via the
# auth.ldap.{users,groups} options (provided by nixos-modules/ldap-auth.nix).
# This config aggregates those declarations from specialArgs.nodes and feeds
# them through the typed `services.nix-hapi-ldap.scopes` schema, which
# translates to the JSON the reconciler consumes.
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

  # The systemd unit name determines the credential directory path.
  credential-path =
    name: "/run/credentials/nix-hapi-ldap.service/${name}-ldap-password-hashed";

  # Credential name -> agenix secret path, one per LDAP user across all hosts.
  user-credentials = lib.mapAttrsToList (
    name: _:
    "${name}-ldap-password-hashed"
    + ":${config.age.secrets."${name}-ldap-password-hashed".path}"
  ) all-ldap-users;

  inherit (flake-inputs.nix-hapi.lib) mkManagedFromPath mkInitialFromPath;

in
{
  imports = [
    ./ldap-auth-facts.nix
    flake-inputs.nix-hapi-provider-ldap.nixosModules.default
  ];
  networking.dnsAliases = [ "ldap" ];

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

  services.nix-hapi.enable = true;
  services.nix-hapi.package = flake-inputs.nix-hapi.packages.${system}.default;
  services.nix-hapi-ldap = {
    enable = true;
    # Use the consumer's flake-inputs view of the package so that
    # nix-config-private's `overridePkg` (which substitutes the patched
    # build with rewritten Cargo git deps for the gitea-only environment)
    # is what actually runs.
    package = flake-inputs.nix-hapi-provider-ldap.packages.${system}.default;

    scopes."proton-ldap" = {
      provider = {
        url = "ldaps://ldap.${facts.network.domain}";
        baseDn = base-dn;
        bindDn = "cn=admin,${base-dn}";
        bindPassword = mkManagedFromPath "/run/credentials/nix-hapi-ldap.service/ldap-root-pass";
      };

      users = lib.mapAttrs (name: ucfg: {
        cn = ucfg.fullName;
        sn = name;
        mail = ucfg.email;
        userPassword =
          if ucfg.managed then
            mkManagedFromPath (credential-path name)
          else
            mkInitialFromPath (credential-path name);
        description = if ucfg.description != "" then ucfg.description else null;
      }) all-ldap-users;

      groups = lib.mapAttrs (_name: group: {
        description = if group.description != "" then group.description else null;
        members = group.members;
      }) all-ldap-groups;
    };
  };

  # Extend the generated service with LDAP ordering, credentials,
  # hardening, and restart triggers.
  systemd.services.nix-hapi-ldap = {
    after = [
      "openldap.service"
      "run-agenix.d.mount"
      "network-online.target"
    ];
    wants = [
      "openldap.service"
      "run-agenix.d.mount"
      "network-online.target"
    ];
    # Restart when the tree JSON or any LDAP password hash changes.
    restartTriggers = [
      config.services.nix-hapi.jsonFiles.ldap
    ]
    ++ lib.mapAttrsToList (
      name: _: config.age.secrets."${name}-ldap-password-hashed".file
    ) all-ldap-users;
    serviceConfig = {
      RemainAfterExit = true;
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
}
