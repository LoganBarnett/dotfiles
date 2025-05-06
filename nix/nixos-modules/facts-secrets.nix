################################################################################
# Based on users in ./facts.nix, provide secrets for various systems.
#
# It can be accessed via `config.lib.ldap'.
#
# This is mostly an organizational file.  Here we can emit secrets that other
# systems can use without having to pull in the origin of the secret.  For
# example, an Octoprint service that has an LDAP service account needn't pull in
# the LDAP service module to get the LDAP service account secret.  That secret
# is emitted here, agnostic of the actual LDAP service module.
#
# Most of these have a CNAME or host-id prefixed to them.  This allows multiple
# service account secrets the same service account coexist on the same host.
# They are independently accessible by their owning services.
################################################################################
{ config, lib, facts, ... }: let

  # TODO: This is getting repeated.  Consider moving this to our own custom
  # lib.nix, or in the facts.nix file.
  named = key: value: { name = key; } // value;
  nameds = attrs: lib.attrsets.mapAttrsToList named attrs;

  ##
  # Emits an LDAP password secret for the user, bound to the group provided.
  #
  # We want passwords generated for each user in the LDAP system.  For human
  # users, this can be thought of as the initial password.  For service
  # accounts, this is the password they use.  Some smarts need to be added to
  # account for this.
  ldap-password = prefix: group: username: {
    "${prefix}-${username}-ldap-password" = {
      generator.script = "passphrase";
      inherit group;
      # Always specify the rekey file or it goes into a weird directory?
      rekeyFile = ../secrets/${prefix}-${username}-ldap-password.age;
      mode = "0440";
    };
    "${prefix}-${username}-ldap-password-hashed" = {
      inherit group;
      generator = {
        script = "slapd-hashed";
        dependencies = [
          config.age.secrets."${prefix}-${username}-ldap-password"
        ];
      };
      # Always specify the rekey file or it goes into a weird directory?
      rekeyFile = ../secrets/${prefix}-${username}-ldap-password-hashed.age;
      mode = "0440";
    };
  };

  # Emits an attrset of LDAP password secrets for each user, bound to the group
  # provided.
  ldap-passwords = prefix: group: users:
    (lib.lists.foldl
      (a: b: a // b)
      # foldl's documentation is incorrect - it needs a third argument.
      {}
      (builtins.map (u: ldap-password prefix group u.name) (nameds users))
    )
  ;

in {
  lib.ldap = {
    inherit ldap-password ldap-passwords;
  };
  # age.secrets = (ldap-passwords facts.network.users);
}
