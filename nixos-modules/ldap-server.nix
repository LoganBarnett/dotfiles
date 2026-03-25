################################################################################
# OpenLDAP server module.
#
# Configures the OpenLDAP daemon with TLS, overlays, and schema.  Activating
# this module only makes the option available; set
# services.ldap-server.enable = true (via nixos-configs/openldap-facts.nix) to
# actually run the server.
#
# Data population is handled separately by ldap-reconciler, which reads a
# desired-state JSON file generated from facts and reconciles it against the
# live directory.  See nixos-configs/openldap-facts.nix.
################################################################################
{
  config,
  facts,
  lib,
  pkgs,
  ...
}:
let
  ldap-port = 636;
  base-dn = "dc=proton,dc=org";
in
{
  options.services.ldap-server = {
    enable = lib.mkEnableOption "OpenLDAP LDAP server";
  };

  config = lib.mkIf config.services.ldap-server.enable {
    tls.tls-leafs."ldap.${facts.network.domain}" = {
      fqdn = "ldap.${facts.network.domain}";
      ca = config.age.secrets.proton-ca;
    };

    age.secrets = {
      ldap-root-pass = {
        generator.script = "passphrase";
        rekeyFile = ../secrets/ldap-root-pass.age;
        group = "openldap";
        mode = "0440";
      };
      ldap-root-pass-hashed = {
        generator = {
          script = "slapd-hashed";
          dependencies = [
            config.age.secrets.ldap-root-pass
          ];
        };
        rekeyFile = ../secrets/ldap-root-pass-hashed.age;
        group = "openldap";
        mode = "0440";
      };
    };

    services.openldap = {
      enable = true;
      urlList = [
        "ldaps:///"
        "ldapi:///"
      ];
      settings = {
        attrs = {
          olcLogLevel = "acl any conns config stats stats2 trace";
          olcTLSCACertificateFile = "${../secrets/proton-ca.crt}";
          olcTLSCertificateFile = "${../secrets/tls-ldap.proton.crt}";
          olcTLSCertificateKeyFile = "/run/credentials/openldap.service/tls-ldap-key";
          olcTLSCipherSuite = "HIGH:MEDIUM:+3DES:+RC4:+aNULL";
          olcTLSCRLCheck = "none";
          olcTLSVerifyClient = "never";
          olcTLSProtocolMin = "3.1";
        };
        children = {
          "cn=schema".includes = [
            "${pkgs.openldap}/etc/schema/core.ldif"
            "${pkgs.openldap}/etc/schema/cosine.ldif"
            "${pkgs.openldap}/etc/schema/inetorgperson.ldif"
            "${pkgs.openldap}/etc/schema/nis.ldif"
          ];
          "cn=module{0}" = {
            attrs = {
              objectClass = [ "olcModuleList" ];
              cn = "module{0}";
              olcModuleLoad = [
                "{0}dynlist"
                "{1}back_monitor"
                "{2}ppolicy"
                "{3}memberof"
                "{4}refint"
                "{5}argon2"
              ];
            };
          };
          "olcDatabase={-1}frontend" = {
            attrs = {
              objectClass = [
                "olcDatabaseConfig"
                "olcFrontendConfig"
              ];
              olcDatabase = "{-1}frontend";
              # olcPasswordHash must live here (not in cn=config) when the hash
              # scheme is provided by a loadable module such as argon2.
              olcPasswordHash = "{ARGON2}";
              olcAccess = [
                "{0}to * by dn.exact=uidNumber=0+gidNumber=0,cn=peercred,cn=external,cn=auth manage stop by * none stop"
              ];
            };
          };
          "olcDatabase={0}config" = {
            attrs = {
              objectClass = "olcDatabaseConfig";
              olcDatabase = "{0}config";
              olcAccess = [ "{0}to * by * none break" ];
            };
          };
          "olcDatabase={1}mdb" = {
            attrs = {
              objectClass = [
                "olcDatabaseConfig"
                "olcMdbConfig"
              ];
              olcDatabase = "{1}mdb";
              olcDbDirectory = "/var/lib/openldap/data";
              olcDbIndex = [
                "objectClass eq"
                "cn pres,eq"
                "uid pres,eq"
                "sn pres,eq,subany"
                "member pres,eq"
                "memberof pres,eq"
              ];
              olcSuffix = base-dn;
              olcRootDN = "cn=admin,${base-dn}";
              olcRootPW.path = config.age.secrets.ldap-root-pass-hashed.path;
              olcAccess = [
                ''
                  {0}to attrs=userPassword
                                    by self write
                                    by anonymous auth
                                    by * none''
                ''{0}to * by dn.exact="cn=admin,dc=proton,dc=org" write by * read''
                ''
                  {1}to *
                                    by * read''
              ];
            };

            children = {
              "olcOverlay={2}ppolicy".attrs = {
                objectClass = [
                  "olcOverlayConfig"
                  "olcPPolicyConfig"
                  "top"
                ];
                olcOverlay = "{2}ppolicy";
                olcPPolicyHashCleartext = "FALSE";
              };

              "olcOverlay={3}memberof".attrs = {
                objectClass = [
                  "olcOverlayConfig"
                  "olcMemberOf"
                  "top"
                ];
                olcOverlay = "{3}memberof";
                olcMemberOfRefInt = "TRUE";
                olcMemberOfDangling = "error";
                olcMemberOfGroupOC = "groupOfNames";
                olcMemberOfMemberAD = "member";
                olcMemberOfMemberOfAD = "memberOf";
              };

              "olcOverlay={4}refint".attrs = {
                objectClass = [
                  "olcOverlayConfig"
                  "olcRefintConfig"
                  "top"
                ];
                olcOverlay = "{4}refint";
                olcRefintAttribute = [
                  "memberof"
                  "member"
                  "manager"
                  "owner"
                ];
              };
            };
          };
        };
      };
    };

    networking.firewall.allowedTCPPorts = [ ldap-port ];
    networking.firewall.allowedUDPPorts = [ ldap-port ];
    systemd.services.openldap = {
      wants = [ "run-agenix.d.mount" ];
      serviceConfig.LoadCredential = [
        "tls-ldap-key:${config.age.secrets."tls-ldap.proton.key".path}"
      ];
    };
  };
}
