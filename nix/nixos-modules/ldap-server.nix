# Use
# https://github.com/Mic92/dotfiles/blob/main/machines/modules/openldap/default.nix
# as a reference.  Keep in mind this only sets up "schema", and doesn't touch
# declarativeContents.  We'll have to declare that ourselves.  See also
# https://www.reddit.com/r/NixOS/comments/fd04jc/comment/fje9d8n
# for getting things working using LDAP authentication.
{ config, facts, host-id, pkgs, lib, ... }: (let
  join-lines = lines:
    lib.strings.concatStrings
      (lib.strings.intersperse "\n" lines)
  ;
  join-ldap-entities = xs: join-lines (builtins.map (x: "${x}\n") xs);
  named = key: value: { name = key; } // value;
  nameds = attrs: lib.attrsets.mapAttrsToList named attrs;
  # I have not observed that the database is dropped when using
  # declarativeContents.  Instead we must do a run with declarativeContents = {}
  # to flush the values out, and then run again with our desired values.
  # Toggling this allows us to set that without making big comments (which I've
  # noticed don't cleanly get uncommented by Emacs' commenter).
  flush-settings = false;
  user-type = (user:
    if user.type == "person" then
      ''
        objectClass: inetOrgPerson
        objectClass: person
        sn: ${user.name}''
    else if user.type == "service" then
      # RFC 2256 / RFC 4519 recommends this, per:
      # https://serverfault.com/a/968265 and
      # https://datatracker.ietf.org/doc/html/rfc4519#page-21
      # It's intentional not to have things like uid so user queries don't turn
      # up with service accounts  To search for the service, use "cn"..
      # This doesn't include a password though.  So include simpleSecurityObject
      # too.  See https://datatracker.ietf.org/doc/html/rfc4524#page-18 for the
      # specification.
      # I may revisit this because uid is pretty handy for generic code.  I can
      # get that from "account".  For that definition see:
      # https://datatracker.ietf.org/doc/html/rfc4524#section-3.1
      ''
        objectClass: applicationProcess
        objectClass: simpleSecurityObject''
      # Lifted this from here: https://serverfault.com/a/330476
      # These give us the uid and userPassword fields that I thought we desired,
      # but we're doing without it because it's recommended to not use this.
      # ''
      #   objectClass: account
      #   objectClass: simpleSecurityObject
      # ''
    else
      # Hopefully this causes an error or something I can see better, but I
      # don't know if it works yet.
      "objectClass: unknownType"
  );
  group-dn = base-dn: group: "cn=${group},ou=groups,${base-dn}";
  members-of = base-dn: groups: username:
    (join-lines
      (lib.trivial.pipe (nameds groups) [
        (builtins.filter (g: lib.lists.any (m: m == username ) g.members))
        (builtins.map (group: "memberOf: ${group-dn base-dn group.name}"))
      ])
    );
  user-validate = user: id:
    lib.trivial.throwIfNot
      (user ? full-name)
      "User ${lib.generators.toPretty {} user} needs a full-name attribute."
      id
  ;
  ldap-user-password = username:
    # Test with a bogus value.  Space added because this isn't a file.
    # " foobar"
    "< file://${config.age.secrets."${username}-ldap-password-hashed".path}"
  ;
  # Anything rammed up against the colon is potentially a file.
  ldap-user = base-dn: groups: username: user: let
    full-user = named username user;
  in user-validate user ''
    dn: uid=${username},ou=users,${base-dn}
    cn: ${full-user.full-name}
    uid: ${username}
    ou: users
    mail: ${user.email}
    description: ${user.description}
    userPassword:${ldap-user-password username}
    objectClass: inetOrgPerson
    objectClass: person
    sn: ${username}
    ${members-of base-dn groups username}
  '';
  # There are too many field differences between a service account and a user.
  # Rather than maintaining a huge difference between the two, just lump them
  # together as "persons" and I'll use another field to distinguish them.
  # ${user-type (named username user)}

  # This is a standard LDAP port.
  ldap-port = 636;
  base-dn = "dc=proton,dc=org";

  #   dn: cn=${username},ou=users,${base-dn}
  #   objectClass: top
  #   objectClass: person
  #   objectClass: organizationalPerson
  #   sn: ServiceAccount
  #   cn: ${username}
  #   description: ${description}
  #   userPassword: no one would use one two three for five four their password
  #   # sAMAccountName: ${username}

  ldap-group = base-dn: name: group:
    ''
    dn: ${group-dn base-dn name}
    objectClass: groupOfNames
    cn: ${name}
    ou: ${name}
    description: ${group.description}
    ${(join-lines
      (builtins.map
        (user: "member: uid=${user},ou=users,${base-dn}")
        group.members
      )
    )}
  '';


in {
  # imports = [
  #   ./openldap-overlayed-contents.nix
  # ];
  age.secrets = {
    ldap-root-pass = {
      generator.script = "passphrase";
      # Always specify the rekey file or it goes into a weird directory?
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
      # Always specify the rekey file or it goes into a weird directory?
      rekeyFile = ../secrets/ldap-root-pass-hashed.age;
      group = "openldap";
      mode = "0440";
    };
  } // (config.lib.ldap.ldap-passwords "openldap" facts.network.users);

  services.openldap = {
    enable = true;
    urlList = [
      "ldaps:///"
      "ldapi:///"
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
    declarativeContents = if flush-settings then {} else {
      # TODO: The example in the option for declarativeContents is broken.  The
      # "dn=" prefix seems to be throwing it off.  I have not seen other
      # configurations in the wild that uses it.  I should open a ticket about
      # it.
      "${base-dn}" = ''
        dn: ${base-dn}
        objectClass: domain
        dc: proton

        dn: ou=users,${base-dn}
        objectClass: organizationalUnit
        ou: users
        description: Users in the proton network.

        dn: ou=groups,${base-dn}
        objectClass: organizationalUnit
        ou: groups
        description: Groups in the proton network.

        ${join-ldap-entities (
          (lib.attrsets.mapAttrsToList
            (ldap-group base-dn)
            facts.network.groups
          )
          ++ (lib.attrsets.mapAttrsToList
               (ldap-user base-dn facts.network.groups)
               facts.network.users
             )
          )
        }
      ''
      ;
    };

    settings = {
      attrs = {
        # - "acl": Access control?  Authentication stuffs.
        olcLogLevel = "acl any conns config stats stats2 trace";
        # olc means Open LDAP Certificates...?
        olcTLSCACertificateFile = "${../secrets/proton-ca.crt}";
        olcTLSCertificateFile = "${../secrets/tls-${host-id}.crt}";
        olcTLSCertificateKeyFile = config.age.secrets."tls-${host-id}.key".path;
        olcTLSCipherSuite = "HIGH:MEDIUM:+3DES:+RC4:+aNULL";
        olcTLSCRLCheck = "none";
        olcTLSVerifyClient = "never";
        olcTLSProtocolMin = "3.1";
        # olc assumes passwords are hashed by default.  Override this until we
        # can get an openldap generated password that is hashed via `slappasswd
        # -s "myplaintextpassword"`.
        # Even {SHA} seems to be based on something specific to the host.  Just
        # use cleartext until I can figure it out.
        # olcPasswordHash = "{SHA}";
        olcPasswordHash = "{CLEARTEXT}";
      };
      children = {
        # TODO: Document what each of these schemas do.
        "cn=schema".includes = [
          "${pkgs.openldap}/etc/schema/core.ldif"
          "${pkgs.openldap}/etc/schema/cosine.ldif"
          "${pkgs.openldap}/etc/schema/inetorgperson.ldif"
          "${pkgs.openldap}/etc/schema/nis.ldif"
          # Add Kerberos support.
          # "${pkgs.ldap-extra-schemas}/kerberos.ldif"
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
            ];
          };
        };
        "olcDatabase={-1}frontend" = {
          attrs = {
            objectClass = "olcDatabaseConfig";
            olcDatabase = "{-1}frontend";
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
              # Add the index for the Kerberos authentication name field.
              # Kerberos doesn't work for me at the moment.  I need to figure
              # out how to properly import the base image.
              # "krbPrincipalName eq,pres,sub"
              "objectClass eq"
              "cn pres,eq"
              "uid pres,eq"
              "sn pres,eq,subany"
              # Optimize member and memberof lookups.
              "member pres,eq"
              "memberof pres,eq"
            ];
            olcSuffix = base-dn;
            /* your admin account, do not use writeText on a production system */
            olcRootDN = "cn=admin,${base-dn}";
            # Untested.  Should work but no run done yet.
            olcRootPW.path = config.age.secrets.ldap-root-pass-hashed.path;
            olcAccess = [
              /* custom access rules for userPassword attributes */
              ''{0}to attrs=userPassword
                by self write
                by anonymous auth
                by * none''
              ''{0}to * by dn.exact="cn=admin,dc=proton,dc=org" write by * read''
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
              # This might be mucking with me shunting passwords directly in via
              # slapadd.
              olcPPolicyHashCleartext = "FALSE";
            };

            "olcOverlay={3}memberof".attrs = {
              objectClass = [ "olcOverlayConfig" "olcMemberOf" "top" ];
              olcOverlay = "{3}memberof";
              olcMemberOfRefInt = "TRUE";
              # It has been warned that leaving this set to "error" could make
              # it not work well with some poorly implemented LDAP client
              # services.  But I really want to see the errors and might be
              # plagued with one right now.
              # olcMemberOfDangling = "ignore";
              olcMemberOfDangling = "error";
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
  # Run a dummy service that can repair these files.
  systemd.services.openldap-pre-fix-secrets = {
    enable = true;
    after = [ "network.target" ];
    before = [ "openldap.service" ];
    serviceConfig = {
      Type = "oneshot";
      # Remember to use a string form so we get the path and thus copied to the
      # Nix store.  I had some head scratching when this was without quotes, and
      # the path was built but the file never laid down in the store remotely.
      ExecStart = let
        script = pkgs.writeShellScriptBin
          "ldap-password-cleanup"
          (builtins.readFile
            ./ldap-password-cleanup.sh
          );
        users = lib.strings.concatStrings (
          lib.strings.intersperse
            " "
            (lib.attrsets.attrNames facts.network.users)
        );
        # Note the lack of .sh - not my choice but whatever.
      in "${script}/bin/ldap-password-cleanup ${users}";
      # Keeps service as 'active' after it finishes.
      RemainAfterExit = true;
      StandardOutput = "journal";
      StandardError = "journal";
      # Run as the openldap user to get access to the secrets.
      User = "root";
      Group = "root";
    };
  };

  # This approach doesn't work because of infinite recursion.
  # systemd.services.openldap.service.ExecStartPre = [
  #   ''
  #   ls /run/agenix/*-ldap-password-hashed | xargs -I{} sed -i {} -E 's/$//'
  #   ''
  # ]
  #   ++ config.systemd.services.openldap.service.ExecStartPre
  #   ;
  /* ensure openldap is launched after certificates are created */
  # systemd.services.openldap = {
  #   wants = [ "acme-${your-host-name}.service" ];
  #   after = [ "acme-${your-host-name}.service" ];
  # };
})
