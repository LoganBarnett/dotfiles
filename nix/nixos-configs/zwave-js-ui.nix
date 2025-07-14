################################################################################
# Configure zwave-js-ui for managing a Z-Wave controller.
#
# See the hardware directory for including a controller.
#
# zwave-js-ui gives a lot of control over things, but you lose a lot of logs
# (unless maybe you set the log level to "silly", but I haven't confirmed this
# yet).  I think it also uses saner defaults for things, and overall seems to
# actually be active.  By contrast, zwave-js-server (node-zwave-server) does not
# appear to be active anymore.
#
# Keep in mind when provisioning / pairing / including new devices, it can take
# minutes for the "interview" to complete.
#
# This service's configuration structure is woefully undocumented.  It is meant
# to just beat up its configuration constantly.  This configuration, along with
# my copy from nixpkgs, copies the settings file out of the store and into
# /var/lib/zwave-js-ui/settings.json.  You can make changes in the UI and
# inspect/diff the file to see what changed.  Do be mindful to move anything you
# want to keep into this configuration, lest the next restart blow it away.
# At some point I should make a read-only mode to lock things down, but I expect
# I'll be tweaking it for a while.
################################################################################
{ config, host-id, ... }: {
  disabledModules = [
    "services/home-automation/zwave-js-ui.nix"
  ];
  imports = [
    ../nixos-modules/zwave-js-ui.nix
    (import ../nixos-modules/https.nix {
      # This is the default, and assumed in the NixOS module.
      server-port = 8091;
      inherit host-id;
      fqdn = "zwave-js-ui.proton";
    })
  ];
  systemd.services.zwave-js-ui = {
    # serviceConfig = {
    #   Environment = let
    #     dir = "/run/credentials/%n";
    #   in [
    #     "KEY_S0_Legacy=${dir}/legacy"
    #     "KEY_S2_Unauthenticated=${dir}/unauthenticated"
    #     "KEY_S2_Authenticated=${dir}/authenticated"
    #     "KEY_S2_AccessControl=${dir}/access-control"
    #   ];
    # };
  };
  services.zwave-js-ui = {
    enable = true;
    secretsConfigFile = "/run/credentials/zwave-js-ui.service/zwave-js-secret";
    settings2 = {
      logLevel = "debug";
      # See
      # https://github.com/zwave-js/zwave-js-ui/blob/26f2e698354e56b7ec1b82cd3a99e106fedcf923/api/lib/ZwaveClient.ts#L588
      # for the closest thing to a document for some of these settings.
      zwave = {
        serverEnabled = true;
        serverHost = "127.0.0.1";
        serverPort = 3000;
        enableStatistics = false;
        disclaimerVersion = 1;
      };
    } // {
      # This section has settings that are set by the UI on startup.  We'll set
      # them first.
      securityKeysLongRange = {};
      deviceConfigPriorityDir = "/var/lib/zwave-js-ui/config";
      enableSoftReset = true;
      disableChangelog = false;
      # Backup what, exactly?
      backup = {
        storeBackup = false;
        storeCron = "0 0 * * *";
        storeKeep = 7;
        nvmBackup = false;
        nvmBackupOnEvent = false;
        nvmCron = "0 0 * * *";
        nvmKeep = 7;
      };
      # A packet sniffer of sorts.  I'm not even sure I have hardware that can
      # do this, just leave it disabled.
      zniffer = {
        enabled = false;
        port = "";
        logEnabled = true;
        logToFile = true;
        maxFiles = 7;
        securityKeys = {
          S2_Unauthenticated = "";
          S2_Authenticated = "";
          S2_AccessControl = "";
          S0_Legacy = "";
        };
        securityKeysLongRange = {
          S2_Authenticated = "";
          S2_AccessControl = "";
        };
        convertRSSI = false;
      };
      ui = {
        darkMode = true;
        navTabs = false;
        compactMode = false;
        streamerMode = false;
      };
    };
    settings = {};
  };
}
