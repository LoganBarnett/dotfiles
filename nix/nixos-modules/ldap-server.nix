{ host-id }: { config, pkgs, lib, ... }: {
  age.secrets.ldap-root-pass = {
    generator.script = "passphrase";
    # Always specify the rekey file or it goes into a weird directory?
    rekeyFile = ../secrets/ldap-root-pass.age;
  };
  services.openldap = {
    enable = true;
    urlList = [
      "ldaps:///"
    ];
    # See:
    # https://search.nixos.org/options?channel=23.11&show=services.openldap.declarativeContents&from=0&size=50&sort=relevance&type=packages&query=openldap+declarative
    # A quick LDAP cheat sheet:
    # dc: Domain Component -
    # ou: Organizational Unit
    # cn: Common Name
    # dn: Distinguished Name - A unique name to the entry.
    # This has some very good information on the topic:
    # https://www.zytrax.com/books/ldap/ch2/index.html#history
    declarativeContents = {
      "dc=proton,dc=org" = ''
            dn= dn: dc=proton,dc=org
            objectClass: dcObject
            objectClass: organization
            dc: proton
            description: The proton network.
            o: Proton Network

            dn: ou=users,dc=proton,dc=org
            objectClass = organizationalUnit
            ou: users
            description: Users in the proton network.

            dn: logan,ou=users,dc=proton,dc=org
            objectClass: inetOrgPerson
            cn: Logan Barnett
            cn: Logan
            sn: logan
            uid: logan
            mail: logustus@gmail.com
            description: The reason we suffer.
            ou: Administrators
          '';
    };
    settings = {
      attrs = {
        olcLogLevel = "conns config";
        # olc means Open LDAP Certificates...?
        olcTLSCACertificateFile = "${../secrets/proton-ca.crt}";
        olcTLSCertificateFile = "${../secrets/tls-${host-id}.crt}";
        olcTLSCertificateKeyFile = config.age.secrets."tls-${host-id}.key".path;
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
        ];
        "olcDatabase={1}mdb".attrs = {
          objectClass = [ "olcDatabaseConfig" "olcMdbConfig" ];
          olcDatabase = "{1}mdb";
          olcDbDirectory = "/var/lib/openldap/data";
          olcSuffix = "dc=proton,dc=org";
          /* your admin account, do not use writeText on a production system */
          olcRootDN = "cn=admin,dc=proton,dc=org";
          # Untested.  Should work but no run done yet.
          olcRootPW.path = config.age.secrets.ldap-root-pass.path;
          olcAccess = [
            /* custom access rules for userPassword attributes */
            ''{0}to attrs=userPassword
                by self write
                by anonymous auth
                by * none''
            /* allow read on anything else */
            ''{1}to *
                by * read''
          ];

          # TODO: Test this now that these are nested under children for
          # `olcDatabase={1}mdb`.  I just now followed that from the wiki page,
          # where just before I had incorrectly copied it.
          children = {

            "olcOverlay={2}ppolicy".attrs = {
              objectClass = [ "olcOverlayConfig" "olcPPolicyConfig" "top" ];
              olcOverlay = "{2}ppolicy";
              olcPPolicyHashCleartext = "TRUE";
            };

            "olcOverlay={3}memberof".attrs = {
              objectClass = [ "olcOverlayConfig" "olcMemberOf" "top" ];
              olcOverlay = "{3}memberof";
              olcMemberOfRefInt = "TRUE";
              olcMemberOfDangling = "ignore";
              olcMemberOfGroupOC = "groupOfNames";
              olcMemberOfMemberAD = "member";
              olcMemberOfMemberOfAD = "memberOf";
            };

            "olcOverlay={4}refint".attrs = {
              objectClass = [ "olcOverlayConfig" "olcRefintConfig" "top" ];
              olcOverlay = "{4}refint";
              olcRefintAttribute = "memberof member manager owner";
            };

          };
        };
      };
    };
  };
  /* ensure openldap is launched after certificates are created */
  # systemd.services.openldap = {
  #   wants = [ "acme-${your-host-name}.service" ];
  #   after = [ "acme-${your-host-name}.service" ];
  # };
}
