################################################################################
# Provides automatic list updates for Blocky DNS without requiring restarts.
#
# This module wraps the blocky-lists-updater functionality, which downloads and
# aggregates DNS blocklists from remote URLs, watches for local list changes,
# serves them via HTTP, and notifies Blocky to refresh without restart.
#
# The module supports two types of list sources:
# 1. Remote sources: Lists of URLs that get downloaded and merged
# 2. Watch lists: Local domain lists that are monitored for changes
#
# Both trigger Blocky refresh via its /api/lists/refresh endpoint.
################################################################################
{ config, lib, pkgs, ... }: let
  inherit (lib) mkEnableOption mkOption mkIf types;
  cfg = config.services.blocky-lists-updater;

  blocky-lists-updater = pkgs.callPackage ../derivations/blocky-lists-updater.nix {};

  sourcesFormat = pkgs.formats.keyValue {};
in {
  options.services.blocky-lists-updater = {
    enable = mkEnableOption "blocky-lists-updater service";

    blockyUrl = mkOption {
      type = types.str;
      default = "http://localhost:4000";
      description = "Base URL for Blocky API to send refresh requests.";
    };

    webPort = mkOption {
      type = types.port;
      default = 8081;
      description = "Port for the embedded static file server.";
    };

    sources = mkOption {
      type = types.attrsOf (types.listOf types.str);
      default = {};
      example = {
        ads = [
          "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"
          "https://blocklistproject.github.io/Lists/ads.txt"
        ];
        malware = [
          "https://blocklistproject.github.io/Lists/phishing.txt"
        ];
      };
      description = ''
        Attribute set mapping list names to URLs.

        Each attribute creates a source file with URLs to download and merge.
        The aggregated list becomes available at
        http://localhost:''${webPort}/downloaded/''${name}.txt
      '';
    };

    watchLists = mkOption {
      type = types.attrsOf types.lines;
      default = {};
      example = {
        custom-blocks = ''
          evil.example.com
          badsite.net
        '';
      };
      description = ''
        Attribute set of custom domain lists to watch for changes.

        These are local lists that will be monitored for modifications.
        When changed, Blocky will be notified to refresh.
        Available at http://localhost:''${webPort}/watch/''${name}.txt
      '';
    };

    updateInterval = mkOption {
      type = types.int;
      default = 86400;
      description = ''
        Interval in seconds between automatic list downloads.
        Set to 0 to disable periodic updates.
      '';
    };

    initialDelay = mkOption {
      type = types.int;
      default = 60;
      description = ''
        Delay in seconds before first list download after service start.
      '';
    };

    logLevel = mkOption {
      type = types.enum [ "NONE" "ERROR" "WARN" "INFO" "DEBUG" ];
      default = "INFO";
      description = "Log verbosity level.";
    };

    stateDir = mkOption {
      type = types.str;
      default = "/var/lib/blocky-lists-updater";
      description = "State directory for lists and temporary files.";
    };
  };

  config = mkIf cfg.enable {
    systemd.services.blocky-lists-updater = {
      description = "Blocky Lists Updater";
      after = [ "network.target" "blocky.service" ];
      wantedBy = [ "multi-user.target" ];
      wants = [ "blocky.service" ];

      environment = {
        BLU_BLOCKY_URL = cfg.blockyUrl;
        BLU_DESTINATION_FOLDER = "${cfg.stateDir}/web/downloaded";
        BLU_SOURCES_FOLDER = "${cfg.stateDir}/sources";
        BLU_WATCH_FOLDER = "${cfg.stateDir}/web/watch";
        BLU_WEB_FOLDER = "${cfg.stateDir}/web";
        BLU_WEB_PORT = toString cfg.webPort;
        BLU_INTERVAL_SECONDS = toString cfg.updateInterval;
        BLU_INITIAL_DELAY_SECONDS = toString cfg.initialDelay;
        BLU_LOG_LEVEL = cfg.logLevel;
        BLU_LOG_FORMAT = "text";
      };

      preStart = ''
        mkdir -p ${cfg.stateDir}/{sources,web/{downloaded,watch}}

        # Write source files containing URLs to download.
        ${lib.concatStringsSep "\n" (lib.mapAttrsToList (name: urls: ''
          cat > ${cfg.stateDir}/sources/${name}.txt <<'EOF'
          ${lib.concatStringsSep "\n" urls}
          EOF
        '') cfg.sources)}

        # Write watch lists for local monitoring.
        ${lib.concatStringsSep "\n" (lib.mapAttrsToList (name: content: ''
          cat > ${cfg.stateDir}/web/watch/${name}.txt <<'EOF'
          ${content}
          EOF
        '') cfg.watchLists)}
      '';

      serviceConfig = {
        Type = "simple";
        ExecStart = "${blocky-lists-updater}/bin/blocky-lists-updater";
        Restart = "on-failure";
        RestartSec = "30s";
        StateDirectory = "blocky-lists-updater";
        WorkingDirectory = cfg.stateDir;

        # Security hardening.
        DynamicUser = true;
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        NoNewPrivileges = true;
        PrivateDevices = false;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        RestrictAddressFamilies = [ "AF_INET" "AF_INET6" "AF_UNIX" ];
        RestrictNamespaces = true;
        LockPersonality = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        RemoveIPC = true;
        SystemCallArchitectures = "native";

        # Bind mount SSL certificates for HTTPS.
        BindReadOnlyPaths = [
          "/etc/ssl/certs"
          "/etc/static/ssl/certs"
        ];
      };
    };

    # Open firewall port for the web server if needed.
    networking.firewall.allowedTCPPorts = mkIf (cfg.webPort != 0) [ cfg.webPort ];
  };
}
