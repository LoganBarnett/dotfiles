################################################################################
# Declares auth.ldap.users and auth.ldap.groups options so services can
# register their LDAP identities inline.  Importing this module makes the
# options available; declaring values activates them.
#
# For every enabled user the module auto-emits two agenix secrets:
#   <name>-ldap-password         — plaintext passphrase
#   <name>-ldap-password-hashed  — slapd-ready hash derived from the above
#
# The openldap reconciler on silicon aggregates these from all hosts in the
# cluster via specialArgs.nodes and populates the live directory accordingly.
################################################################################
{
  config,
  lib,
  ...
}:
let
  inherit (lib) mkOption types;
  inherit (types)
    attrsOf
    bool
    enum
    listOf
    str
    submodule
    ;
in
{
  options.auth.ldap = {
    users = mkOption {
      type = attrsOf (submodule {
        options = {
          enable = lib.mkEnableOption "LDAP user" // {
            default = true;
          };
          email = mkOption {
            type = str;
            description = "Email address for this LDAP user.";
          };
          fullName = mkOption {
            type = str;
            description = "Display name for this LDAP user.";
          };
          description = mkOption {
            type = str;
            default = "";
            description = "Optional description.  Empty string omits the attribute.";
          };
          type = mkOption {
            type = enum [
              "person"
              "service"
            ];
            default = "service";
            description = "Whether this is a human or service account.";
          };
          group = mkOption {
            type = str;
            default = "root";
            description = "Unix group that owns the generated secret files.";
          };
          managed = mkOption {
            type = bool;
            default = true;
            description = ''
              When true the reconciler keeps the password in sync on every run.
              When false it sets the password only on creation, then leaves it
              alone — appropriate for human accounts whose passwords change
              interactively.
            '';
          };
        };
      });
      default = { };
      description = "LDAP users declared by this host.";
    };

    groups = mkOption {
      type = attrsOf (submodule {
        options = {
          description = mkOption {
            type = str;
            default = "";
            description = "Optional group description.";
          };
          members = mkOption {
            type = listOf str;
            default = [ ];
            description = "LDAP usernames that are members of this group.";
          };
        };
      });
      default = { };
      description = "LDAP groups contributed by this host.";
    };
  };

  config.age.secrets = lib.mkMerge (
    lib.mapAttrsToList (
      name: ucfg:
      lib.optionalAttrs ucfg.enable {
        "${name}-ldap-password" = {
          generator.script = "passphrase";
          group = ucfg.group;
          shared = true;
          rekeyFile = ../secrets/${name}-ldap-password.age;
          mode = "0440";
        };
        "${name}-ldap-password-hashed" = {
          group = ucfg.group;
          shared = true;
          generator = {
            script = "slapd-hashed";
            dependencies = [ config.age.secrets."${name}-ldap-password" ];
          };
          rekeyFile = ../secrets/${name}-ldap-password-hashed.age;
          mode = "0440";
        };
      }
    ) config.auth.ldap.users
  );
}
