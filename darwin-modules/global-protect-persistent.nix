################################################################################
# GlobalProtect VPN persistent connection service for nix-darwin.
# Automatically connects and maintains VPN connection using gp-connect-auto.
# Runs as a system daemon (root) to allow VPN tunnel creation while accessing
# the primary user's gpg keys and pass password store for authentication.
#
# Also provides a local dnsmasq forwarder on 127.0.0.1:53 so that Nix-compiled
# tools (curl with c-ares, git, dig) can resolve VPN domains through
# /etc/resolv.conf instead of relying on macOS /etc/resolver/ split-DNS.
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
  dnsCfg = cfg.dnsmasq;

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

  dnsmasqConf = pkgs.writeText "dnsmasq.conf" ''
    # Listen only on localhost so dnsmasq does not conflict with
    # mDNSResponder on the wildcard address.
    listen-address=127.0.0.1
    bind-interfaces

    # Upstream DNS discovered by dnsmasq-upstream-sync via DHCP.
    # dnsmasq polls this file for changes automatically.
    resolv-file=${dnsCfg.upstreamResolvFile}

    # VPN domain-specific servers written by vpnc-script-macos on connect
    # and cleared on disconnect.  Reloaded on SIGHUP.
    servers-file=${dnsCfg.vpnServersFile}

    # Static domain forwarding (e.g. proton -> home DNS).
    ${concatMapStringsSep "\n" (
      e: "server=/${e.domain}/${e.server}"
    ) dnsCfg.domainForwarding}

    # Sane defaults for a local forwarder.
    cache-size=${toString dnsCfg.cacheSize}
    domain-needed
    bogus-priv
  '';

  dnsmasqUpstreamSync =
    pkgs.callPackage ../derivations/dnsmasq-upstream-sync/default.nix
      { };

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

      dnsmasq = {
        domainForwarding = mkOption {
          type = types.listOf (
            types.submodule {
              options = {
                domain = mkOption {
                  type = types.str;
                  description = "Domain to forward (e.g. proton).";
                };
                server = mkOption {
                  type = types.str;
                  description = "DNS server IP for this domain.";
                };
              };
            }
          );
          default = [ ];
          example = [
            {
              domain = "proton";
              server = "192.168.254.9";
            }
          ];
          description = "Static domain-specific DNS forwarding entries.";
        };

        vpnServersFile = mkOption {
          type = types.str;
          default = "/var/run/dnsmasq-vpn-servers.conf";
          description = ''
            Path for dynamic VPN domain servers written by vpnc-script-macos
            on connect and cleared on disconnect.
          '';
        };

        upstreamResolvFile = mkOption {
          type = types.str;
          default = "/var/run/dnsmasq-upstream-resolv.conf";
          description = ''
            Path for upstream resolv.conf written by dnsmasq-upstream-sync
            from DHCP-provided DNS.
          '';
        };

        cacheSize = mkOption {
          type = types.int;
          default = 1000;
          description = "Number of DNS cache entries for dnsmasq.";
        };

        syncInterval = mkOption {
          type = types.int;
          default = 30;
          description = ''
            Seconds between upstream DNS discovery checks by
            dnsmasq-upstream-sync.
          '';
        };
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
      pkgs.dnsmasq
      dnsmasqUpstreamSync
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
      ${cfg.primaryUser} ALL=(root) NOPASSWD: /usr/sbin/networksetup -setdnsservers * 127.0.0.1
      ${cfg.primaryUser} ALL=(root) NOPASSWD: /usr/sbin/networksetup -setsearchdomains * Empty
      ${cfg.primaryUser} ALL=(root) NOPASSWD: /usr/sbin/networksetup -setwebproxystate * Off
      ${cfg.primaryUser} ALL=(root) NOPASSWD: /usr/sbin/networksetup -setsecurewebproxystate * Off
      ${cfg.primaryUser} ALL=(root) NOPASSWD: /usr/sbin/networksetup -setv6automatic *
      ${cfg.primaryUser} ALL=(root) NOPASSWD: /usr/sbin/networksetup -setproxybypassdomains * Empty
      ${cfg.primaryUser} ALL=(root) NOPASSWD: /usr/sbin/networksetup -setnetworkserviceenabled * Off
      ${cfg.primaryUser} ALL=(root) NOPASSWD: /usr/sbin/networksetup -setnetworkserviceenabled * On
      ${cfg.primaryUser} ALL=(root) NOPASSWD: /usr/bin/dscacheutil -flushcache
      ${cfg.primaryUser} ALL=(root) NOPASSWD: /usr/bin/killall -HUP mDNSResponder
      ${cfg.primaryUser} ALL=(root) NOPASSWD: /usr/bin/killall -HUP dnsmasq
      # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      # !! DO NOT COMMIT - TEMPORARY DEVELOPMENT RULE - REMOVE BEFORE MERGE !!
      # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ${cfg.primaryUser} ALL=(root) NOPASSWD: SETENV: ${
        pkgs.callPackage ../derivations/vpn-test-harness.nix { }
      }/bin/vpn-test-harness *
    '';

    # Ensure dnsmasq data files exist before dnsmasq starts, and point
    # all network services' DNS to 127.0.0.1 so every tool (Nix and
    # native) resolves through dnsmasq.
    system.activationScripts.postActivation.text =
      let
        vpnServersFile = dnsCfg.vpnServersFile;
        upstreamResolvFile = dnsCfg.upstreamResolvFile;
      in
      ''
        # dnsmasq data files: touch so dnsmasq does not fail on first boot.
        if [ ! -f "${vpnServersFile}" ]; then
          touch "${vpnServersFile}"
        fi
        if [ ! -f "${upstreamResolvFile}" ]; then
          # Seed with a sane default so DNS works before the first sync.
          echo "nameserver 192.168.254.9" > "${upstreamResolvFile}"
        fi

        # Point all network services' DNS to 127.0.0.1 (dnsmasq).
        /usr/sbin/networksetup -listallnetworkservices | tail -n +2 | while IFS= read -r svc; do
          /usr/sbin/networksetup -setdnsservers "$svc" 127.0.0.1 2>/dev/null || true
        done
      '';

    # dnsmasq local DNS forwarder — always active when the module is
    # imported.  VPN DNS resolution is inseparable from the VPN service.
    launchd.daemons.dnsmasq = {
      serviceConfig = {
        KeepAlive = true;
        RunAtLoad = true;
        ProgramArguments = [
          "${pkgs.dnsmasq}/bin/dnsmasq"
          "--keep-in-foreground"
          "--conf-file=${dnsmasqConf}"
        ];
        StandardOutPath = "/var/log/dnsmasq.log";
        StandardErrorPath = "/var/log/dnsmasq.log";
      };
    };

    # Companion daemon that discovers DHCP DNS from the active interface
    # and writes it to the upstream resolv file for dnsmasq.
    launchd.daemons.dnsmasq-upstream-sync = {
      serviceConfig = {
        KeepAlive = true;
        RunAtLoad = true;
        ProgramArguments = [
          "${dnsmasqUpstreamSync}/bin/dnsmasq-upstream-sync"
        ];
        EnvironmentVariables = {
          DNSMASQ_UPSTREAM_RESOLV_FILE = dnsCfg.upstreamResolvFile;
          DNSMASQ_SYNC_INTERVAL = toString dnsCfg.syncInterval;
        };
        StandardOutPath = "/var/log/dnsmasq-upstream-sync.log";
        StandardErrorPath = "/var/log/dnsmasq-upstream-sync.log";
      };
    };

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
