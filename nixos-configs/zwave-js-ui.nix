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
{ config, host-id, lib, pkgs, ... }: {
  disabledModules = [
    "services/home-automation/zwave-js-ui.nix"
  ];
  imports = [
    ../nixos-modules/zwave-js-ui.nix
  ];
  https.fqdns."zwave-js-ui.proton" = {
    enable = true;
    internalPort = 8091;
  };
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
    package = pkgs.zwave-js-ui.overrideAttrs (let
      version = "10.9.0";
    in old: rec {
      inherit version;
      src = pkgs.fetchFromGitHub {
        owner = "LoganBarnett";
        repo = "zwave-js-ui";
        rev = "fix-settings-circular-on-logging-override";
        # hash = lib.fakeHash;
        hash = "sha256-QYIfrkirCdIvuvy8vqEB+NgwijTWFUGWlvHcNucjtwo=";
      };
      # overrideAttrs and buildNpmPackage don't play together.  This has to be
      # overridden in npmDeps as well.
      # Don't actually worry about setting this.
      # npmDepsHash = lib.fakeHash;
      npmDeps = pkgs.fetchNpmDeps {
        inherit src;
        # hash = lib.fakeHash;
        hash = "sha256-E3+6N04R02Re/AIMQu1l5PlZrwpnfEQJGVf7CzXo8zw=";
      };
    });
    secretsConfigFile = "/run/credentials/zwave-js-ui.service/zwave-js-secret";
    settings = {
      # See
      # https://github.com/zwave-js/zwave-js-ui/blob/26f2e698354e56b7ec1b82cd3a99e106fedcf923/api/lib/ZwaveClient.ts#L588
      # for the closest thing to a document for some of these settings.
      zwave = {
        commandsTimeout = 30;
        # logLevel = "debug";
        # logEnabled = true;
        # Prevent it from logging the good stuff to a file where the systemd
        # journal doesn't see it, and also keeps us from having to worry about
        # log rotation outside of the journal.
        # logToFile = false;
        serverEnabled = true;
        serverHost = "127.0.0.1";
        serverPort = 3000;
        enableStatistics = false;
        disclaimerVersion = 1;
        # Per the document, this is where the secret settings show up, which win
        # out over the documented settings.  See
        # https://zwave-js.github.io/zwave-js/#/api/driver?id=zwaveoptions for
        # them just pasting their blasted TypeScript interfaces into an HTML
        # file, which is supposed to stand for documentation.
        # There's some additional mention of it here:
        # https://github.com/zwave-js/zwave-js-ui/discussions/2166#discussioncomment-1948873
        # In that comment it is suggested that all logging values should be
        # declared here and not a mishmash of here and in the root of zwave.
        # Also when logging settings appear in both locations, the UI becomes
        # unusable because it cannot serialize the JSON needed to supply
        # settings to the UI.
        options = {
          # Force it to log everything to the console and not just some things.
          logConfig = {
            # Which is it?
            enable = true;
            enabled = true;
            forceConsole = true;
            # 6 is "silly" for reference.  5 is debug.
            level = 5;
          };
        };
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
    # settings = {};
  };
}
