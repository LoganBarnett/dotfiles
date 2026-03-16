################################################################################
# This file declares all of the access and capabilities of various users on my
# network.  This is to keep the access control completely abstract.  See the
# various adapter files in this directory for how these get transformed into
# declarations that work with the various permission systems.
#
# I might favor a generalized system (such as LDAP) which many systems support,
# but there might be gaps or I might want to switch to another generalized
# system.  In either case, this declaration here should never be changed - only
# the adapters.
#
# Adapters should know what permissions belong to them without the need for
# adapter-specific decoration.  For example, assume the general system is LDAP,
# and there is a foo-service where we wish to perform bar operations for the
# user qux.  foo-service doesn't use LDAP.  Regardless of the setup (and
# including this setup), we would simply call the permission "bar" or even
# "foo-bar" to indicate that the "bar" operation applies to "foo", but there's
# no mention of LDAP here, or any other system that might be involved.  The
# "foo" naming should _not_ apply to an adapter, even if we do have to make an
# adapter just for "foo".  This might become relevant if we have have to support
# a non-LDAP generalized permission system alongside LDAP.
#
# This kind of permission system seems fairly rudimentary - it has no scoping
# capabilities but that would imply the systems it is applied to are also ready
# for scoping.  Generally they are not.
#
# This file should be considered sensitive to external viewers, but during its
# infancy I can keep a copy public.  When I migrate it privately, I can leave an
# example copy here, and document how one would use it.
################################################################################
{

  permissions = {

  };

  roles = {
    gitea-admin = {
      permissions = [

      ];
    };
  };

  users = {
    logan = {
      roles = [
        "gitea-admin"
      ];
    };
  };

}
