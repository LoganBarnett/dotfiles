##
# NextCloud is a fork of OwnCloud - an open source self-hosted "cloud" suite.
# It offers calendar, email, task management, video calls, file storage, and a
# number of other features (typically via apps).
#
# See https://nixos.wiki/wiki/Nextcloud for getting started.
# See
# https://nixos.org/manual/nixos/stable/index.html#module-services-nextcloud-basic-usage
# for the official documentation (though it isn't much of a reference).
{ hostname }: {
  config,
  lib,
  pkgs,
  ...
}: {
  age.secrets.nextcloud-admin-password = {
    rekeyFile = ../secrets/nextcloud-admin-pass.age;
    # See secrets.nix for where this is declared.
    generator = "long-passphrase";
  };
  services.nextcloud = {
    enable = true;
    hostName = hostname;
    config.adminpassFile = config.age.secrets.nextcloud-admin-password.path;
    extraApps = {
      inherit (config.services.nextcloud.package.packages.apps)
        calendar
        contacts
        news
        tasks
        ;
    };
    https = true;
    extraAppsEnable = true;
    # HEIC must be explicitly supported, so we need the original list plus HEIC.
    # TODO: Determine how to query the default, and simply add to it.
    extraOptions.enabledPreviewProviders = [
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
  services.nginx.virtualHosts.${config.services.nextcloud.hostName} = {
    forceSSL = true;
    # TODO: What's this do?
    enableACME = true;
  };
  networking.hostName = hostname;
}
