# Use
# https://github.com/Mic92/dotfiles/blob/main/machines/modules/openldap/default.nix
# as a reference.  Keep in mind this only sets up "schema", and doesn't touch
# declarativeContents.  We'll have to declare that ourselves.  See also
# https://www.reddit.com/r/NixOS/comments/fd04jc/comment/fje9d8n
# for getting things working using LDAP authentication.
{ host-id }: { config, pkgs, lib, ... }: (let
  # This is a standard LDAP port.
  ldap-port = 636;
  membersOf = membership: "foobar";
  ldapHumanUser = base-dn: {
    username,
    full-name,
    email,
    description ? "",
    membership,
  }: ''
    dn: uid=${username},ou=users,${base-dn}
    objectClass: inetOrgPerson
    cn: ${full-name}
    cn: ${username}
    uid: ${username}
    mail: ${email}
    description: ${description}
    membersOf: ${membersOf membership}
  '';
  ldapServiceUser = base-dn: {
    username,
    full-name,
    email,
    description,
    membership,
  }: ''
    dn: uid=${username},ou=users,${base-dn}
    objectClass: use something else!
    cn: ${full-name}
    cn: ${username}
    uid: ${username}
    mail: ${email}
    description: ${description}
    membersOf: ${membersOf membership}
  '';
in {
  age.secrets.ldap-root-pass = {
    generator.script = "passphrase";
    # Always specify the rekey file or it goes into a weird directory?
    rekeyFile = ../secrets/ldap-root-pass.age;
    group = "openldap";
    mode = "0440";
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
      # TODO: The example in the option for declarativeContents is broken.  The
      # "dn=" prefix seems to be throwing it off.  I have not seen other
      # configurations in the wild that uses it.  I should open a ticket about
      # it.
      "dc=proton,dc=org" = ''
        dn: dc=proton,dc=org
        objectClass: domain
        dc: proton

        dn: ou=users,dc=proton,dc=org
        objectClass: organizationalUnit
        ou: users
        description: Users in the proton network.

        dn: uid=logan,ou=users,dc=proton,dc=org
        objectClass: inetOrgPerson
        cn: Logan Barnett
        cn: Logan
        sn: logan
        uid: logan
        mail: logustus@gmail.com
        description: The reason we suffer.
        ou: Administrators
        userPassword: foobar
      '';
    };

    settings = {
      attrs = {
        # - "acl": Access control?  Authentication stuffs.
        olcLogLevel = "acl conns config";
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
        # TODO: Document what each of these schemas do.
        "cn=schema".includes = [
          "${pkgs.openldap}/etc/schema/core.ldif"
          "${pkgs.openldap}/etc/schema/cosine.ldif"
          "${pkgs.openldap}/etc/schema/inetorgperson.ldif"
          "${pkgs.openldap}/etc/schema/nis.ldif"
        ];
        "olcDatabase={-1}frontend" = {
          attrs = {
            objectClass = "olcDatabaseConfig";
            olcDatabase = "{-1}frontend";
            olcAccess = [ "{0}to * by dn.exact=uidNumber=0+gidNumber=0,cn=peercred,cn=external,cn=auth manage stop by * none stop" ];
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
              # Examples from Mic92.  I don't know what these do yet.
          #     ''
          #   {0}to attrs=userPassword
          #                  by self write  by anonymous auth
          #                  by dn.base="cn=dovecot,dc=mail,dc=eve" read
          #                  by dn.base="cn=gitlab,ou=system,ou=users,dc=eve" read
          #                  by dn.base="cn=ldapsync,ou=system,ou=users,dc=eve"
          #                  read by * none''
          # ''{1}to attrs=loginShell  by self write  by * read''
          # ''
          #   {2}to dn.subtree="ou=system,ou=users,dc=eve"
          #                  by dn.base="cn=dovecot,dc=mail,dc=eve" read
          #                  by dn.subtree="ou=system,ou=users,dc=eve" read
          #                  by * none''
          # ''{3}to dn.subtree="ou=jabber,ou=users,dc=eve"  by dn.base="cn=prosody,ou=system,ou=users,dc=eve" write  by * read''
          # ''{4}to * by * read''
            ];
          };

          children = {

            # ppolicy is no longer needed?  I've read some things indicating
            # it's not needed, but this doesn't error so who knows.
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
              # TODO: Update documentation to use list instead of space
              # separated string.
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
  users.users.openldap = {
    extraGroups = [ "tls-leaf" ];
  };
  networking.firewall.allowedTCPPorts = [ ldap-port ];
  networking.firewall.allowedUDPPorts = [ ldap-port ];
  /* ensure openldap is launched after certificates are created */
  # systemd.services.openldap = {
  #   wants = [ "acme-${your-host-name}.service" ];
  #   after = [ "acme-${your-host-name}.service" ];
  # };
})
