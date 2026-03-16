################################################################################
# GlobalProtect VPN persistent connection service for nix-darwin.
# Automatically connects and maintains VPN connection using gp-connect-auto.
# Runs as a system daemon (root) to allow VPN tunnel creation while accessing
# the primary user's gpg keys and pass password store for authentication.
################################################################################
{
  config,
  lib,
  pkgs,
  ...
}:

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

    # Create log directory (running as user, ownership is automatic)
    mkdir -p "$GP_LOG_DIR"

    # Run the monitor script
    exec ${cfg.package}/bin/gp-monitor
  '';

in
{
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
        default = "/Users/${cfg.primaryUser}/.local/share/gpclient/logs";
        example = "/Users/john.doe/.local/share/gpclient/logs";
        description = "Directory where VPN monitor logs will be stored.";
      };

      headlessBrowser = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Whether to run the Chromium browser in headless mode during
          authentication.  Set to false to show the browser window for
          debugging SSO flows.
        '';
      };

      package = mkOption {
        type = types.package;
        default = pkgs.callPackage ../derivations/gp-monitor.nix { };
        description = "The gp-monitor package to use.";
      };
    };
  };

  config = {

    # Packages and sudoers rules are always available when the module is
    # imported, regardless of whether the daemon is enabled.  This ensures
    # the VPN tooling is present even when iterating with enable = false.

    environment.systemPackages = [
      (pkgs.callPackage ../derivations/cleanup-vpn.nix { })
      (pkgs.callPackage ../derivations/dns-resolver-helper.nix { })
      (pkgs.callPackage ../derivations/dns-vpn-scoping-fix.nix { })
      pkgs.gpclient
      (pkgs.callPackage ../derivations/gp-connect-auto.nix { })
      (pkgs.callPackage ../derivations/test-vpn-connectivity.nix { })
      (pkgs.callPackage ../derivations/vpn-test-harness-recover.nix { })
      (pkgs.callPackage ../derivations/vpn-test-harness.nix { })
    ];

    # Allow user to run specific commands without password for VPN automation.
    # 1. dns-resolver-helper: validates inputs and only operates on /etc/resolver
    # 2. gp-connect-auto: needs elevated privileges for tunnel creation
    # 3. cleanup-vpn: TEMPORARY for development - TODO: REMOVE BEFORE MERGE
    # 4. vpn-test-harness-recover: restores default gateway after VPN tunnel collapse
    security.sudo.extraConfig = ''
      ${cfg.primaryUser} ALL=(root) NOPASSWD: ${
        pkgs.callPackage ../derivations/dns-resolver-helper.nix { }
      }/bin/dns-resolver-helper *
      ${cfg.primaryUser} ALL=(root) NOPASSWD: SETENV: ${
        pkgs.callPackage ../derivations/gp-connect-auto.nix { }
      }/bin/gp-connect-auto
      ${cfg.primaryUser} ALL=(root) NOPASSWD: ${
        pkgs.callPackage ../derivations/cleanup-vpn.nix { }
      }/bin/cleanup-vpn
      ${cfg.primaryUser} ALL=(root) NOPASSWD: SETENV: ${
        pkgs.callPackage ../derivations/vpn-test-harness-recover.nix { }
      }/bin/vpn-test-harness-recover
      # Allow manual DHCP renewal and network reset for post-VPN-disconnect recovery.
      ${cfg.primaryUser} ALL=(root) NOPASSWD: /usr/sbin/ipconfig set * DHCP
      ${cfg.primaryUser} ALL=(root) NOPASSWD: /usr/sbin/networksetup -setdhcp *
      ${cfg.primaryUser} ALL=(root) NOPASSWD: /usr/sbin/networksetup -setdnsservers * Empty
      ${cfg.primaryUser} ALL=(root) NOPASSWD: /usr/sbin/networksetup -setsearchdomains * Empty
      ${cfg.primaryUser} ALL=(root) NOPASSWD: /usr/sbin/networksetup -setwebproxystate * Off
      ${cfg.primaryUser} ALL=(root) NOPASSWD: /usr/sbin/networksetup -setsecurewebproxystate * Off
      ${cfg.primaryUser} ALL=(root) NOPASSWD: /usr/sbin/networksetup -setv6automatic *
      ${cfg.primaryUser} ALL=(root) NOPASSWD: /usr/sbin/networksetup -setproxybypassdomains * Empty
      ${cfg.primaryUser} ALL=(root) NOPASSWD: /usr/sbin/networksetup -setnetworkserviceenabled * Off
      ${cfg.primaryUser} ALL=(root) NOPASSWD: /usr/sbin/networksetup -setnetworkserviceenabled * On
      ${cfg.primaryUser} ALL=(root) NOPASSWD: /usr/bin/dscacheutil -flushcache
      ${cfg.primaryUser} ALL=(root) NOPASSWD: /usr/bin/killall -HUP mDNSResponder
      # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      # !! DO NOT COMMIT - TEMPORARY DEVELOPMENT RULE - REMOVE BEFORE MERGE !!
      # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ${cfg.primaryUser} ALL=(root) NOPASSWD: SETENV: ${
        pkgs.callPackage ../derivations/vpn-test-harness.nix { }
      }/bin/vpn-test-harness *
    '';

    # The launchd daemon is only created when explicitly enabled.
    launchd.daemons.globalprotect-monitor = mkIf cfg.enable {
      path = [ config.environment.systemPath ];

      serviceConfig = {
        KeepAlive = true;
        RunAtLoad = true;
        ProgramArguments = [
          "${pkgs.bash}/bin/bash"
          "${monitorScript}"
        ];

        # Run as primary user
        UserName = cfg.primaryUser;
        GroupName = "staff";

        # Environment variables - these match what gp-connect-auto expects
        EnvironmentVariables = {
          GP_SERVER = cfg.server;
          GP_USERNAME = cfg.username;
          ORG_NAME = cfg.orgName;
          GP_CHECK_INTERVAL = toString cfg.checkInterval;
          GP_LOG_DIR = cfg.logDir;
          GP_BROWSER_HEADLESS = if cfg.headlessBrowser then "true" else "false";
        }
        // (optionalAttrs (cfg.gateway != null) {
          GP_GATEWAY = cfg.gateway;
        });

        # Logging
        StandardOutPath = "${cfg.logDir}/launchd-stdout.log";
        StandardErrorPath = "${cfg.logDir}/launchd-stderr.log";
      };
    };
  };
}
