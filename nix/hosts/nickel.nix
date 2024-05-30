################################################################################
# This defines the entirety of the configuration for the nickel host.
#
# Nickel is tasked as an LDAP server to declare access and permission.
################################################################################
{ disko-proper, flake-inputs }: let
  host-id = "nickel";
  system = "aarch64-linux";
in {
  inherit system;
  modules = [
    # We can't use `disko` because it's taken, I guess.
    disko-proper.nixosModules.disko
    ../hacks/installer/installation-cd-minimal.nix
    ../hacks/installer/cd-dvd-channel.nix
    (import ../nixos-modules/server-host.nix {
      inherit disko-proper flake-inputs host-id;
    })
    ({ config, pkgs, ... }: {
      services.openldap = {
        enable = true;
        # suffix = "dc=proton,dc=org";
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
          # "dc=proton,dc=org" = ''
          #   dn= dn: dc=proton,dc=org
          #   objectClass: dcObject
          #   objectClass: organization
          #   dc: proton
          #   description: The proton network.
          #   o: Proton Network

          #   dn: ou=users,dc=proton,dc=org
          #   objectClass = organizationalUnit
          #   ou: users
          #   description: Users in the proton network.

          #   dn: logan,ou=users,dc=proton,dc=org
          #   objectClass: inetOrgPerson
          #   cn: Logan Barnett
          #   cn: Logan
          #   sn: logan
          #   uid: logan
          #   mail: logustus@gmail.com
          #   description: The reason we suffer.
          #   ou: Administrators
          # '';
        };
        settings = {
        # TODO: Fix documentation - this doesn't exist.
      # TODO: Fix documentation - this doesn't exist.
        attrs = {
          olcLogLevel = "conns config";
          # olc means Open LDAP Certificates...?
          olcTLSCACertificateFile = ../secrets/proton-ca.crt;
          olcTLSCertificateFile = ../secrets/tls-${host-id}.crt;
          olcTLSCertificateKeyFile = config.age.secrets."tls-${host-id}.key".path;
          olcTLSCipherSuite = "HIGH:MEDIUM:+3DES:+RC4:+aNULL";
          olcTLSCRLCheck = "none";
          olcTLSVerifyClient = "never";
          olcTLSProtocolMin = "3.1";
        };
        # TODO: Fix documentation - this doesn't exist.
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
            olcSuffix = "dc=example,dc=com";
            /* your admin account, do not use writeText on a production system */
            olcRootDN = "cn=admin,dc=example,dc=com";
            olcRootPW.path = pkgs.writeText "olcRootPW" "pass";
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
          };
        };
        };
      };
      /* ensure openldap is launched after certificates are created */
      # systemd.services.openldap = {
      #   wants = [ "acme-${your-host-name}.service" ];
      #   after = [ "acme-${your-host-name}.service" ];
      # };
    })
    ({ lib, ... }: {
      disko.devices = {
        disk.disk1 = {
          device = lib.mkDefault "/dev/sda";
          type = "disk";
          content = {
            # Required for MBR.  Despite the "BIOS" (or whatever the
            # confirguration is for the motherboard) saying "UEFI", it's either
            # in a compatibility mode that I can't figure out how to change, or
            # is flat out incorrect.  Use `sudo parted /dev/sda` to load that
            # disk, and then `p` to inspect it.
            type = "gpt"; # Grub Partition Table?
            partitions = {
              boot = {
                name = "boot";
                size = "1M";
                type = "EF02"; # 02 is Grub's MBR.
                # type = "EF00"; # 02 is UEFI.
              };
              # Why is this here in the example?  Leaving this out seems to make
              # a GPT+BIOS boot actually work.
              # esp = {
              #   name = "ESP";
              #   size = "500M";
              #   type = "EF00";
              #   content = {
              #     type = "filesystem";
              #     format = "vfat";
              #     mountpoint = "/boot";
              #   };
              # };
              root = {
                name = "root";
                size = "100%";
                content = {
                  type = "lvm_pv";
                  vg = "pool";
                };
              };
            };
          };
        };
        # TODO: It would be good to put a swap partition in place.
        lvm_vg = {
          pool = {
            type = "lvm_vg";
            lvs = {
              root = {
                size = "100%FREE";
                content = {
                  type = "filesystem";
                  format = "ext4";
                  mountpoint = "/";
                  mountOptions = [
                    "defaults"
                  ];
                };
              };
            };
          };
        };
      };
    })
  ];
}
