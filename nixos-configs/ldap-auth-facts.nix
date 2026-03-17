################################################################################
# Maps human users from facts.nix into auth.ldap.users and auth.ldap.groups.
#
# Imported by openldap-facts.nix so that person accounts reach the LDAP
# reconciler.  Service accounts are declared inline by each service's own
# config, so this file only handles the person type.
#
# Each person lists their LDAP group memberships under ldap-groups.  The merge
# here builds up auth.ldap.groups by emitting one { members = [ name ]; }
# fragment per (person, group) pair; lib.mkMerge combines them so every group
# accumulates all its members regardless of declaration order.
################################################################################
{
  facts,
  lib,
  ...
}:
let
  persons = lib.filterAttrs (_: u: u.type == "person") facts.network.users;
in
{
  auth.ldap.users = lib.mapAttrs (_: user: {
    email = user.email;
    fullName = user.full-name;
    description = user.description;
    type = "person";
    group = "root";
    # Person passwords are set once on LDAP entry creation and then left to
    # the user to change.  The reconciler must not overwrite them.
    managed = false;
  }) persons;

  auth.ldap.groups = lib.mkMerge (
    lib.mapAttrsToList (
      name: user:
      lib.listToAttrs (
        map (g: lib.nameValuePair g { members = [ name ]; }) (user.ldap-groups or [ ])
      )
    ) persons
  );
}
