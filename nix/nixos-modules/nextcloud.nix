################################################################################
# NextCloud is a fork of OwnCloud - an open source self-hosted "cloud" suite.
# It offers calendar, email, task management, video calls, file storage, and a
# number of other features (typically via apps).
#
# Some Nextcloud + NixOS resources:
# - https://nixos.wiki/wiki/Nextcloud
# - https://nixos.org/manual/nixos/stable/#module-services-nextcloud
# - https://github.com/NixOS/nixpkgs/blob/master/pkgs/servers/nextcloud/packages/README.md
# See
# https://nixos.org/manual/nixos/stable/index.html#module-services-nextcloud-basic-usage
# for the official documentation (though it isn't much of a reference).
#
# Use `nextcloud-occ` for the `occ` command.  Here's an exmaple:
# sudo -u nextcloud nextcloud-occ ldap:show-config
#
# See
# https://github.com/NixOS/nixpkgs/blob/master/pkgs/servers/nextcloud/packages/README.md
# can be used to emit new packages from the app store.  It uses
# https://github.com/helsinki-systems/nc4nix to do the conversion.  Why not just
# do them all?  In any case, it needs a fix.  It works fine on macOS.  I should
# submit a pull request to nixpkgs to fix that.  Adding "user_ldap" to the JSON
# file didn't work - nothing new got emitted.  Might require some debugging.
# The "user_ldap" app is somewhat special because it lives in Nextcloud itself:
# https://github.com/nextcloud/server/tree/master/apps/user_ldap
# The official Nextcloud LDAP docs are here:
# https://docs.nextcloud.com/server/latest/admin_manual/configuration_user/user_auth_ldap.html
#
# You can see a listing of apps on https://apps.nextcloud.com or locally via
# https://nextcloud.proton/settings/apps.
#
# This setup might seem a little janky but I have to fight with systemd quite a
# bit to make it robust and able to survive a reboot.  In essence, there's been
# a lot of work in making sure the system mounts and mounts again (once for NFS,
# the other for limited permissions) before anything that needs to _use_ those
# mounts gets to run.  If those mounts aren't in place, these processes will
# still write something but it will be wrong.  Thus you will see some belt and
# suspender stuff - checks that are redundant, but I found helpful when trying
# to get everything working.  I'd like to leave it in place unless these
# (possibly) redundant checks prove problematic in some way.
################################################################################
{
  config,
  host-id,
  lib,
  pkgs,
  ...
}: let
  fqdn = "nextcloud.proton";
  data-dir = "/mnt/nextcloud-data";
  nfs-physical-hostname = "silicon.proton";
  nfs-vpn-hostname = "silicon-nas.proton";
in {
  imports = [
    # (import ../nixos-modules/https.nix {
    #   inherit fqdn host-id;
    #   redirect = false;
    # })
    ../nixos-modules/bindfs.nix
    ../nixos-modules/nfs-mount-consumer.nix
  ];
  age.secrets = {
    nextcloud-nfs-wireguard-key = {
      generator.script = "wireguard-priv";
      group = "nextcloud";
      mode = "0440";
      rekeyFile = ../secrets/nextcloud-nfs-wireguard-key.age;
    };
    nextcloud-admin-password = {
      # See secrets.nix for where this is declared.
      generator.script = "long-passphrase";
      # Due to startup ordering issues, this might need to be done manually with
      # chgrp.  That being said, some systemd unit dependency declaration
      # ("wants agenix" perhaps?) could fix this.
      group = "nextcloud";
      mode = "0440";
      rekeyFile = ../secrets/nextcloud-admin-pass.age;
    };
  } // (
    config.lib.ldap.ldap-password
      "openldap-${host-id}-nextcloud-service"
      "${host-id}-nextcloud-service"
  );
  environment.systemPackages = [
    # If you had to reinstall Nextcloud, you've probably quickly learned that it
    # gets into a lodged state where any number of tiny little things that
    # shouldn't be a problem actually serve to lodge it further in mysterious
    # and nonsensical ways.  This option is somewhat nuclear - you'll lose any
    # and all "Nextcloud data", but it will retain your files your users have at
    # least.  Except for admin.  But we do keep a backup of admin in case you
    # need something.
    (pkgs.writeShellApplication {
      name = "nextcloud-clean-for-install";
      text = ''
        rm ${data-dir}/config/config.php || true
        rm ${data-dir}/config/override.config.php || true
        mv ${data-dir}/data/${
          config.services.nextcloud.config.adminuser
        }{,.bak-"$(date '+%s')"} || true
        # We could wipe the entire server but there might be other services
        # using PostgreSQL.  So just wipe the nextcloud database.
        sudo -u postgres psql -c "DROP DATABASE nextcloud;" || true
        sudo -u postgres psql -c "CREATE DATABASE nextcloud OWNER nextcloud;" \
          || true
      '';
    })
  ];
  services.https.fqdns."nextcloud.proton" = {
    enable = true;
    proxy = false;
  };
  services.nfs-mount.mounts.nextcloud = {
    enable = true;
    bindToService = "nextcloud";
    remoteHost = "silicon.proton";
    vpnHost = "silicon-nas.proton";
    share = "/tank/data/nextcloud";
    mountPoint = data-dir;
    wgPrivateKeyFile = config.age.secrets."${host-id}-nfs-wireguard-key".path;
    wgIP = "10.100.0.3/24";
    wgPeerPublicKeyFile = ../secrets/generated/silicon-nfs-wireguard-key.pub;
    wgPeerIP = "10.100.0.1/32";
    wgInterfaceName = "wgfs-silicon";
  };
  services.nextcloud = {
    enable = true;
    # Be mindful that Nextcloud doesn't support upgrades over multiple versions,
    # so you'll need to walk it forward slowly if upgrades are to come in the
    # future.
    # As of writing, pkgs.nextcloud evaluates to v27 which nixpkgs has marked as
    # broken.  v30 is the latest, but there is a nar hash mismatch with the
    # contacts app.
    # error: hash mismatch in fixed-output derivation '/nix/store/vz304hjdg8rqn691mi0146sf07gngs9w-contacts-v6.1.2.tar.gz.drv':
    #          specified: sha256-M3AC9KT3aMpDYeGgfqVWdI4Lngg/yw/36HSBS3N+G5c=
    #             got:    sha256-Slk10WZfUQGsYnruBR5APSiuBd3jh3WG1GIqKhTUdfU=
    # error: 1 dependencies of derivation '/nix/store/3lk2jrxjzvgk07xdqzacxbj0ql37g6y8-nextcloud-app-contacts-6.1.1.drv' failed to build
    # error: 1 dependencies of derivation '/nix/store/ddd9xzy4clnq9hvxwvl9yap3silk467a-nix-apps.drv' failed to build
    # error: 1 dependencies of derivation '/nix/store/p1vf696xgb4z39gf62sx0a76n3hd3yj2-nextcloud-30.0.2-with-apps.drv' failed to build
    package = pkgs.nextcloud31;
    database.createLocally = true;
    datadir = data-dir;
    hostName = fqdn;
    # This needs to be set in conjunction with the https custom module I have.
    # This is because PHP runs on nginx, so instead of reverse proxying to a
    # whole new process, it just uses HTTPS directly.
    https = true;
    config = {
      # This defaults to root.  It's pretty much documented everywhere as
      # "admin".
      # adminuser = "root";
      adminuser = "admin";
      adminpassFile = config.age.secrets.nextcloud-admin-password.path;
      # dbtype = "sqlite";
      dbtype = "pgsql";
      dbuser = "nextcloud";
      dbname = "nextcloud";
      # The documentation says this should be a path to the socket, but it
      # really should be a path to the socket's base directory.  The socket n
      # name is derived from the standard socket name template that PostgreSQL
      # uses, including converting its TCP port into part of the path.
      dbhost = "/run/postgresql";
      # dbpass = "";
      # This shouldn't be needed because of PostgreSQL's peer authentication.
      # In essence: The Unix user name must match.
      # dbpassFile = config.age.secrets.nextcloud-database-password.path;
    };
    phpOptions = {
      "opcache.enable" = "1";
      "opcache.enable_cli" = "1";
      "opcache.memory_consumption" = "128";
      "opcache.interned_strings_buffer" = "16";
      "opcache.max_accelerated_files" = "10000";
      "opcache.validate_timestamps" = "1";
      "opcache.revalidate_freq" = "60";
    };
    # See `nextcloud-custom-config` in this document for how some other built-in
    # apps are "installed".
    extraApps = {
      inherit (config.services.nextcloud.package.packages.apps)
        calendar
        # Problematic, due to narhash mismatch.
        # contacts
        news
        tasks
        ;
    };
    # Enables the extra-apps above.
    extraAppsEnable = true;
    # This might get us into trouble but at least it'll let me look around at
    # things more, I hope.
    # appstoreEnable = true;
    # It got us into trouble.  I suppose it didn't survive an update.
    appstoreEnable = false;
    settings = {
      # Unfortunately setting this to true will break the pre-start service.
      # Setting it to false will make other, internal machinations in Nextcloud
      # break because it wants this to be set to true if the file is read-only
      # (which Nix will make read-only).
      # config_is_read_only = true;
      # I can't seem to find the LDAP app per
      # https://docs.nextcloud.com/server/latest/admin_manual/configuration_user/user_auth_ldap.html
      # in the app store listing, but there is mention of it here:
      # https://github.com/nextcloud/server/tree/dd66231a90873c750665343b46d3fb6f96826616/apps/user_ldap
      # Since it's not technically an app from the store, I'm not sure how to
      # install it in Nix, but there are built-ins I think, and this may be one
      # of them as well.  For now I've decided to just make the app work and
      # I'll see if the instructions prove useful in finding a built-in app
      # already installed.
      # extraApps = {
      #   user_ldap =
      # };
      # HEIC must be explicitly supported, so we need the original list plus
      # HEIC.
      # TODO: Determine how to query the default, and simply add to it.
      enabledPreviewProviders = [
        "OC\\Preview\\BMP"
        "OC\\Preview\\GIF"
        "OC\\Preview\\JPEG"
        "OC\\Preview\\Krita"
        "OC\\Preview\\MarkDown"
        "OC\\Preview\\MP3"
        "OC\\Preview\\OpenDocument"
        "OC\\Preview\\PNG"
        "OC\\Preview\\TXT"
        "OC\\Preview\\XBitmap"
        "OC\\Preview\\HEIC"
      ];
      # I've seen snippets of this, but it hasn't worked.  Perhaps I had the
      # structure wrong?  See the nextcloud-custom-config service in this file
      # for the real configuration.
      # user_ldap = {};
    };
  };
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_16;
    ensureDatabases = [ "nextcloud" ];
    ensureUsers = [
      {
        # This uses peer authentication by default, which means only users of
        # matching Unix user names can connect as this user.
        name = "nextcloud";
        ensureDBOwnership = true;
      }
    ];
  };
  systemd.services = {
    phpfpm-nextcloud = let
      after = [
        "bindfs-nextcloud.service"
        "postgresql.service"
        "run-agenix.d.mount"
      ];
    in {
      # Make it so Nextcloud can always find its admin password.
      inherit after;
      requires = after;
      serviceConfig = {
        BindPaths = data-dir;
        LoadCredential = [
          "nextcloud-service-ldap-password:${
            config.age.secrets."${host-id}-nextcloud-service-ldap-password".path
          }"
        ];
      };
    };
    # This is so sticky, so put a check in place to ensure it's happening
    # correctly.  We _really_ don't want to be reading and writing to the wrong
    # directory that doesn't get backed up and doesn't even host our files we
    # want.
    nextcloud-bindfs-check = let
      after = [ "bindfs-nextcloud.service" ];
      # List them all in case we take out the custom config one day.
      before = [
        "nextcloud-setup.service"
        "nextcloud-custom-config.service"
        "nextcloud.service"
      ];
    in {
      serviceConfig = {
        AssertPathIsMountPoint = "${data-dir}/data";
      };
      after = after;
      requires = after;
      before = before;
      wantedBy = before;
      # This file was placed to ensure we're looking at the right directory,
      # regardless of mount settings and all that.
      script = ''stat "${data-dir}/data/nfs-share-working"'';
    };
    # To do "dynamic" configuration like we do here, see:
    # https://wiki.nixos.org/wiki/Nextcloud#Dynamic_configuration
    # This is required because we don't have another way to enable user_ldap.
    # The pkgs/servers/nextcloud/packages/generate.sh script does not generate
    # `user_ldap`.  `user_ldap` exists in Nextcloud proper as seen here:
    # https://github.com/nextcloud/server/tree/master/apps/user_ldap
    # or if you need a specific commit:
    # https://github.com/nextcloud/server/tree/8886f367e433277cf7aa0c01b93a9d4348db47a8/apps/user_ldap
    # TODO: Use services.nextcloud.settings to configure this plugin.  I should
    # be able to copy the values.
    nextcloud-custom-config = let
      after = [
        "run-agenix.d.mount"
        "bindfs-nextcloud.service"
        "nextcloud-setup.service"
      ];
    in {
      path = [];
      serviceConfig = {
        LoadCredential = [
          "nextcloud-service-ldap-password:${
            config.age.secrets."${host-id}-nextcloud-service-ldap-password".path
          }"
        ];
      };
      # Most of this is set from doing "nextcloud-occ ldap:show-config" after
      # having clicked around in the UI.
      script = let
        uid = "uid=${host-id}-nextcloud-service,ou=users,dc=proton,dc=org";
        passwordPath = "/run/credentials/nextcloud-custom-config.service/nextcloud-service-ldap-password";
        ldapHost = "nickel.proton";
        script = pkgs.writeShellApplication {
          name = "nextcloud-ldap-configure";
          runtimeInputs = [
            config.services.nextcloud.occ
            pkgs.openldap
          ];
          text = ''
            set -e
            # None of this is going to work if we can't do a simple bind.
            ldapwhoami -x \
              -D "${uid}" \
              -w "$(cat "${passwordPath}")" \
              -H ldaps://${ldapHost}
            set="nextcloud-occ ldap:set-config s01"
            nextcloud-occ app:enable user_ldap
            if nextcloud-occ ldap:show-config s01 ; then
              echo "Configuration exists.  But going to set values anyways..."
            else
              echo "Prior configuration does not exist.  Creating now..."
              nextcloud-occ ldap:create-empty-config
            fi
            $set ldapHost "ldaps://${ldapHost}"
            $set ldapPort "636"
            $set ldapBase "dc=proton,dc=org"
            $set ldapBaseGroups "dc=proton,dc=org"
            $set ldapBaseUsers "dc=proton,dc=org"
            $set ldapAgentName "${uid}"
            $set ldapAgentPassword "$(cat "${passwordPath}")"
            $set ldapAttributesForUserSearch "uid"
            $set ldapExpertUsernameAttr "uid"
            $set ldapLoginFilter "(&(&(|(objectclass=inetOrgPerson))(|(memberof=cn=nextcloud-users,ou=groups,dc=proton,dc=org)))(uid=%uid))"
            $set ldapLoginFilterEmail "0"
            $set ldapLoginFilterMode "1"
            $set ldapLoginFilterUsername "1"
            # We probably want to revisit this at some point.
            $set ldapNestedGroups "0"
            $set ldapPagingSize "500"
            $set ldapAdminGroup "nextcloud-admins"
            $set ldapUserFilter "(&(|(objectclass=inetOrgPerson))(|(memberof=cn=nextcloud-admins,ou=groups,dc=proton,dc=org)(memberof=cn=nextcloud-users,ou=groups,dc=proton,dc=org)))"
            $set ldapUserFilterGroups "nextcloud-admins;nextcloud-users"
            $set ldapUserFilterMode "1"
            $set ldapUserFilterObjectClass "inetOrgPerson"
            $set ldapUserAvatarRule "default"
            $set ldapUserDisplayName "cn"
            $set ldapUuidGroupAttribute "auto"
            $set ldapUuidUserAttribute "auto"
            # Prevents NextCloud from using the UUID username form on disk, and
            # helps keep things very tidy and consistent.
            $set internalUsernameAttribute "uid"
            $set markRemnantsAsDisabled "0"
            $set turnOnPasswordChange "0"
            $set useMemberOfToDetectMembership "0"
            $set lastJpegPhotoLookup "0"
            $set hasMemberOfFilterSupport "1"
            $set ldapGroupFilter "(&(|(objectclass=groupOfNames))(|(cn=nextcloud-admins)(cn=nextcloud-users)))"
            $set ldapGroupFilterGroups "nextcloud-admins;nextcloud-users"
            $set ldapGroupFilterMode "1"
            $set ldapGroupFilterObjectClass "groupOfNames"
            $set ldapGroupMemberAssocAttr "member"
            $set ldapGroupDisplayName "cn"
            $set ldapCacheTTL "600"
            $set ldapGidNumber "gidnumber"
            $set turnOffCertCheck "0"
            $set ldapExperiencedAdmin "0"
            $set ldapConnectionTimeout "15"
            $set ldapConfigurationActive "1"
            nextcloud-occ ldap:test-config s01
            nextcloud-occ ldap:show-config s01
          '';
        };
      in ''
        ${script}/bin/nextcloud-ldap-configure
      '';
      inherit after;
      requires = after;
      before = [ "multi-user.target" "phpfpm-nextcloud.service" ];
      wantedBy = [ "multi-user.target" "phpfpm-nextcloud.service" ];
    };
    bindfs-nextcloud = let
      before = [
        "nextcloud-setup.service"
        "nextcloud-update-db.service"
        "nextcloud-custom-config.service"
        "nextcloud.service"
      ];
    in {
      before = before;
      wantedBy = before;
    };
  };
  users.users.nextcloud = {
    # This is typically _not_ set by the Nextcloud NixOS module, but we're
    # doing a lot of hand-off that requires permissions to align.
    isSystemUser = true;
    extraGroups = [
      "openldap-${host-id}-nextcloud-service"
    ];
    group = "nextcloud";
  };
  users.groups.nextcloud = {};
  users.groups."openldap-${host-id}-nextcloud-service" = {};
  # TODO: Move to `../nixos-modules/nfs-mount-consumer.nix`.
  networking.hosts = {
    # Give us a fake hostname so we can ensure traffic flows through the VPN,
    # otherwise the NFS IP protection will drop the traffic.
    "10.100.0.1" = [ nfs-vpn-hostname ];
  };
}
