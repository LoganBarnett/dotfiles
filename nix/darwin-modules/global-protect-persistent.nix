################################################################################
# GlobalProtect VPN persistent connection service for nix-darwin.
# Automatically connects and maintains VPN connection using gp-connect-auto.
# Runs as a system daemon (root) to allow VPN tunnel creation while accessing
# the primary user's gpg keys and pass password store for authentication.
################################################################################
{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.services.globalprotect-monitor;

  monitorScript = pkgs.writeShellScript "gp-monitor-wrapper" ''
    set -e

    # Configuration from Nix options - these match what gp-connect-auto expects
    export GP_SERVER="${cfg.server}"
    ${optionalString (cfg.gateway != null) ''export GP_GATEWAY="${cfg.gateway}"''}
    export GP_USERNAME="${cfg.username}"
    export ORG_NAME="${cfg.orgName}"
    export GP_CHECK_INTERVAL="${toString cfg.checkInterval}"
    export GP_LOG_DIR="${cfg.logDir}"

    # Create log directory
    mkdir -p "$GP_LOG_DIR"

    # Run the monitor script
    exec ${cfg.package}/bin/gp-monitor
  '';

in {
  options = {
    services.globalprotect-monitor = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable the GlobalProtect VPN monitor service.";
      };

      server = mkOption {
        type = types.str;
        example = "vpn.company.com";
        description = "The GlobalProtect portal server URL.";
      };

      gateway = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "gateway.company.com";
        description = ''
          The GlobalProtect gateway to connect to.
          If not specified, the portal will prompt for gateway selection.
        '';
      };

      username = mkOption {
        type = types.str;
        example = "john.doe@company.com";
        description = "The VPN username/email to use for authentication.";
      };

      orgName = mkOption {
        type = types.str;
        example = "company";
        description = ''
          The organization name used for pass password store entries.
          This is used to retrieve credentials from pass (e.g., 'pass show <orgName>').
        '';
      };

      primaryUser = mkOption {
        type = types.str;
        default = "logan.barnett";
        example = "john.doe";
        description = ''
          The primary user whose gpg keys and pass store will be used.
          The service runs as root but accesses this user's credentials.
        '';
      };

      checkInterval = mkOption {
        type = types.int;
        default = 60;
        example = 120;
        description = ''
          Seconds between VPN connection checks.
          The service will check if the VPN is connected every N seconds.
        '';
      };

      reconnectTimeout = mkOption {
        type = types.int;
        default = 300;
        example = 600;
        description = ''
          Reconnection retry timeout in seconds.
          This is passed to gpclient's --reconnect-timeout option.
        '';
      };

      logDir = mkOption {
        type = types.str;
        default = "/var/log/gpclient";
        example = "/var/log/gpclient";
        description = "Directory where VPN monitor logs will be stored.";
      };

      package = mkOption {
        type = types.package;
        default = pkgs.callPackage ../derivations/gp-monitor.nix {};
        description = "The gp-monitor package to use.";
      };
    };
  };

  config = mkIf cfg.enable {

    # Ensure required packages are available
    environment.systemPackages = [
      pkgs.gpclient
      (pkgs.callPackage ../derivations/gp-connect-auto.nix {})
    ];

    # Create the launchd system daemon (runs as root for VPN tunnel creation)
    launchd.daemons.globalprotect-monitor = {
      path = [ config.environment.systemPath ];

      serviceConfig = {
        KeepAlive = true;
        RunAtLoad = true;
        ProgramArguments = [ "${pkgs.bash}/bin/bash" "${monitorScript}" ];

        # Run as root to allow VPN tunnel creation
        UserName = "root";
        GroupName = "wheel";

        # Environment variables - these match what gp-connect-auto expects
        EnvironmentVariables = {
          GP_SERVER = cfg.server;
          GP_USERNAME = cfg.username;
          ORG_NAME = cfg.orgName;
          GP_CHECK_INTERVAL = toString cfg.checkInterval;
          GP_LOG_DIR = cfg.logDir;
          # Use primary user's HOME for gpg/pass access (service runs as root)
          HOME = "/Users/${cfg.primaryUser}";
        } // (optionalAttrs (cfg.gateway != null) {
          GP_GATEWAY = cfg.gateway;
        });

        # Logging
        StandardOutPath = "${cfg.logDir}/launchd-stdout.log";
        StandardErrorPath = "${cfg.logDir}/launchd-stderr.log";
      };
    };
  };
}
