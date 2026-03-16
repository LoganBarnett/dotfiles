{ lib, pkgs, ... }:
{
  # Produces an Argon2id hash of an LDAP password suitable for use as a
  # userPassword attribute value.  The argon2 module must be loaded in
  # OpenLDAP's cn=config for verification to succeed at runtime.
  #
  # The trailing newline from slappasswd is stripped here.  If it were
  # preserved it would be included in the base64-encoded hash value (visible
  # as a trailing "Cg" or "Cg=="), causing every authentication attempt to
  # fail silently.
  age.generators.slapd-hashed =
    {
      decrypt,
      deps,
      pkgs,
      ...
    }:
    ''
      ${pkgs.openldap}/bin/slappasswd \
        -o module-load=${pkgs.openldap}/lib/modules/argon2.so \
        -h '{ARGON2}' \
        -s "$(${decrypt} ${lib.escapeShellArg (builtins.elemAt deps 0).file})" \
        | tr -d '\n'
    '';
}
