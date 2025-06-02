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
# https://gallium.proton/settings/apps.
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
    (import ../nixos-modules/https.nix {
      inherit fqdn host-id;
      redirect = false;
    })
    ../nixos-modules/wireguard-agenix-rekey-generator.nix
  ];
  age.secrets = if config.services.nextcloud.enable then ({
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
  )) else {};
  fileSystems."${data-dir}" = {
    device = "${nfs-vpn-hostname}:/tank/data/nextcloud";
    fsType = "nfs";
    options = [
      "defaults"
      "noatime"
      "x-systemd.requires=network-online.target"
      "x-systemd.after=network-online.target"
      "_netdev"
      # "x-systemd.after=wireguard-wg0.service"
      # "x-systemd.requires=wireguard-wg0.service"
    ];
    # Important!  Don't block boot if it's unavailable.
    neededForBoot = false;
  };
  systemd.automounts = [
    {
      where = "mnt-nextcloud-data";
      wantedBy = [ "nextcloud.service" ];
      automountConfig.TimeoutIdleSec = "600";
    }
  ];
  systemd.mounts = [
    {
      what = "${nfs-vpn-hostname}:/tank/data/nextcloud";
      type = "nfs";
      where = "/mnt/nextcloud-data";
      options = "defaults,noatime,_netdev,x-systemd.requires=network-online.target,x-systemd.after=network-online.target";
    }
  ];
  systemd.network.wait-online.enable = true;
  networking.wireguard.enable = true;
  networking.wireguard.interfaces.wg0 = {
    ips = [ "10.100.0.3/24" ];
    privateKeyFile = config.age.secrets.nextcloud-nfs-wireguard-key.path;
    peers = [
      {
        publicKey = builtins.readFile ../secrets/silicon-nfs-wireguard-key.pub;
        endpoint = "${nfs-physical-hostname}:51820";
        allowedIPs = [ "10.100.0.1/32" ];
        persistentKeepalive = 25;
      }
    ];
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
    datadir = data-dir;
    hostName = fqdn;
    # This needs to be set in conjunction with the https custom module I have.
    # This is because PHP runs on nginx, so instead of reverse proxying to a
    # whole new process, it just uses HTTPS directly.
    https = true;
    config = {
      adminpassFile = config.age.secrets.nextcloud-admin-password.path;
      dbtype = "sqlite";
      # This defaults to root.  It's pretty much documented everywhere as
      # "admin".
      # adminuser = "root";
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
    appstoreEnable = true;
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
  systemd.services = {
    phpfpm-nextcloud = {
      # Make it so Nextcloud can always find its admin password.
      after = [ "run-agenix.d.mount" ];
      serviceConfig = {
        LoadCredential = [
          "nextcloud-service-ldap-password:${
            config.age.secrets."${host-id}-nextcloud-service-ldap-password".path
          }"
        ];
      };
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
    nextcloud-custom-config = {
      path = [
        config.services.nextcloud.occ
      ];
      serviceConfig = {
        LoadCredential = [
          "nextcloud-service-ldap-password:${
            config.age.secrets."${host-id}-nextcloud-service-ldap-password".path
          }"
        ];
      };
      # Most of this is set from doing "nextcloud-occ ldap:show-config" after
      # having clicked around in the UI.
      script = ''
        set="nextcloud-occ ldap:set-config s01"
        nextcloud-occ app:enable user_ldap
        # I originally had a call to ldap:create-empty-config but it seemed to
        # always create a new configuration regardless.  No surprises there, but
        # undesirable.  Unsure if it's even needed if I know what I want the
        # exact config to be, as is below.  If not, I'll have to re-add it with
        # a guard so this service can be ephemeral.
        $set ldapHost "ldaps://nickel.proton"
        $set ldapPort "636"
        $set ldapBase "dc=proton,dc=org"
        $set ldapBaseGroups "dc=proton,dc=org"
        $set ldapBaseUsers "dc=proton,dc=org"
        $set ldapAgentName "uid=${host-id}-nextcloud-service,ou=users,dc=proton,dc=org"
        $set ldapAgentPassword "$(cat /run/credentials/nextcloud-custom-config.service/nextcloud-service-ldap-password)"
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
      before = [ "phpfpm-nextcloud.service" ];
      wantedBy = [ "multi-user.target" ];
    };
  };
  systemd.tmpfiles.rules = [
    "d ${data-dir} 0770 nextcloud nextcloud -"
  ];
  users.users.nextcloud.extraGroups = [
    "openldap-${host-id}-nextcloud-service"
  ];
  users.groups."openldap-${host-id}-nextcloud-service" = {};
  networking.hosts = {
    # Give us a fake hostname so we can ensure traffic flows through the VPN,
    # otherwise the NFS IP protection will drop the traffic.
    "10.100.0.1" = [ nfs-vpn-hostname ];
  };
}
