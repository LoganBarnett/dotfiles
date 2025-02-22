##
# NextCloud is a fork of OwnCloud - an open source self-hosted "cloud" suite.
# It offers calendar, email, task management, video calls, file storage, and a
# number of other features (typically via apps).
#
# See https://nixos.wiki/wiki/Nextcloud for getting started.
# See
# https://nixos.org/manual/nixos/stable/index.html#module-services-nextcloud-basic-usage
# for the official documentation (though it isn't much of a reference).
#
# Use `nextcloud-occ` for the `occ` command.  Here's an exmaple:
# sudo -u nextcloud nextcloud-occ ldap:show-config
{
  config,
  host-id,
  lib,
  pkgs,
  ...
}: let
  fqdn = "${host-id}.proton";
in {
  imports = [
    (import ../nixos-modules/https.nix {
      inherit fqdn host-id;
      redirect = false;
    })
  ];
  age.secrets = if config.services.nextcloud.enable then ({
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
      "nextcloud"
      "gallium-nextcloud-service"
  )) else {};
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
    package = pkgs.nextcloud30;
    hostName = fqdn;
    # This needs to be set in conjunction with the https custom module I have.
    # This is because PHP runs on nginx, so instead of reverse proxying to a
    # whole new process, it just uses HTTPS directly.
    https = true;
    config = {
      adminpassFile = config.age.secrets.nextcloud-admin-password.path;
      # This defaults to root.  It's pretty much documented everywhere as
      # "admin".
      # adminuser = "root";
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
    # I can't seem to find the LDAP app per
    # https://docs.nextcloud.com/server/latest/admin_manual/configuration_user/user_auth_ldap.html
    # in the app store listing, but there is mention of it here:
    # https://github.com/nextcloud/server/tree/dd66231a90873c750665343b46d3fb6f96826616/apps/user_ldap
    # Since it's not technically an app from the store, I'm not sure how to
    # install it in Nix, but there are built-ins I think, and this may be one of
    # them as well.  For now I've decided to just make the app work and I'll see
    # if the instructions prove useful in finding a built-in app already
    # installed.
    # extraApps = {
    #   user_ldap =
    # };
    # HEIC must be explicitly supported, so we need the original list plus HEIC.
    # TODO: Determine how to query the default, and simply add to it.
    settings.enabledPreviewProviders = [
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
  };
  systemd.services = if config.services.nextcloud.enable then {
    phpfpm-nextcloud = {
    # Make it so Nextcloud can always find its admin password.
    after = [ "run-agenix.d.mount" ];
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
      script = ''
        nextcloud-occ app:enable user_ldap
      '';
      after = [ "nextcloud-setup.service" ];
      wantedBy = [ "multi-user.target" ];
    };
  } else {};
}
