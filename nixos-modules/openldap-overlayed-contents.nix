################################################################################
# I used
# https://stackoverflow.com/questions/48723492/how-can-i-hide-the-definition-of-a-nixos-option-in-one-module-if-it-is-composed
# as an example.
#
# This is still very much a work in progress, and might be abandoned because I
# got things working with a more sophisticated LDAP configuration.
#
# This module adds an `overlayedContents` field to the OpenLDAP module wherein
# overlays get a chance to apply to the LDAP entities.
#
# The OpenLDAP module for NixOS has a handy `declartiveContents` field that can
# be used to declare the contents of the LDAP database declaratively, but that
# field gets applied via slapadd.  This means that contents are not added with
# overlay information, and so much of the utility is lost to me.
#
# This code can also serve as an example of how to splice in one's own options
# to an existing module without having to copy all of it.
################################################################################
{ config, lib, nixpkgs, pkgs, ... }@args: let
  ##############################################################################
  # These are copied from the original for convenience.
  ##############################################################################
  cfg = config.services.openldap;
  openldap = cfg.package;
  configDir = if cfg.configDir != null then cfg.configDir else "/etc/openldap/slapd.d";
  original =
    import "${nixpkgs}/nixos/modules/services/databases/openldap.nix" args;
  custom = {
    # Declare the new option here.  This follows the internals of the original
    # module.
    options.services.openldap = {
      overlayedContents = lib.mkOption {
        type = with lib.types; attrsOf lines;
        default = {};
        description = ''
          Declarative contents for the LDAP database, in LDIF format by suffix,
          but overlays are applied to these.

          Note that the root of the DB must be defined in
          `services.openldap.settings` and the
          `olcDbDirectory` must begin with
          `"/var/lib/openldap"`.
        '';
        example = lib.literalExpression ''
          {
            "dc=example,dc=org" = '''
              dn= dn: dc=example,dc=org
              objectClass: domain
              dc: example

             dn: ou=users,dc=example,dc=org
              objectClass = organizationalUnit
              ou: users

             # ...
            ''';
          }
        '';
      };
    };
    config.systemd.services.openldap.serviceConfig.ExecStartPre = let
      # These functions are all copies of the original, since they are local
      # variables where they are used.
      dbSettings = lib.mapAttrs' (name: { attrs, ... }: lib.nameValuePair attrs.olcSuffix attrs) (
        lib.filterAttrs (
          name: { attrs, ... }: (lib.hasPrefix "olcDatabase=" name) && attrs ? olcSuffix
        ) cfg.settings.children
      );
      contentsFiles = lib.mapAttrs
        (dn: ldif: pkgs.writeText "${dn}-overlayed.ldif" ldif)
        cfg.overlayedContents;
      # Renamed to disambiguate.  This is different in that it uses ldapadd
      # instead of slapadd.
      writeOverlayedContents = pkgs.writeShellScript "openldap-load-overlayed" ''
        set -euo pipefail

        # rm -rf $2/*
        echo "$3" | ${openldap}/bin/ldapadd \
          -H ldapi:/// \
          -F ${configDir} \
          -D "$1"  \
          -y "$2"
      '';
    in config.systemd.services.openldap.serviceConfig.ExecStartPre
       ++ (lib.mapAttrsToList (
         dn: content:
         lib.escapeShellArgs [
           writeOverlayedContents
           cfg.olcRootDN      # $1
           cfg.olcRootPW.path # $2
           # dn
           # (lib.getAttr dn dbSettings).olcDbDirectory
           content            # $3
         ]
       ) contentsFiles)
    ;
  };
in {
  # Disable the original module, since we imported it by hand.
  disabledModules = [ "services/databases/openldap.nix" ];
  options = lib.recursiveUpdate original.options custom;
  config = lib.recursiveUpdate original.config custom;
}
