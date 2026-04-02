################################################################################
# Darwin launchd user agent that monitors host reachability and plays a local
# alert when all watched hosts have been continuously unreachable for
# downtimeThreshold seconds.
#
# Runs as a user agent (not a daemon) so it has access to the audio session
# for sound output.
################################################################################
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.hostConnectivityMonitor;

  alert-me-locally =
    pkgs.callPackage ../derivations/alert-me-locally/default.nix
      { };
  host-connectivity-monitor =
    pkgs.callPackage ../derivations/host-connectivity-monitor/default.nix
      { };

in
{
  options.services.hostConnectivityMonitor = {
    enable = lib.mkEnableOption "host connectivity monitor";

    hosts = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      example = [
        "192.168.1.1"
        "1.1.1.1"
      ];
      description = ''
        Hosts to monitor.  An alert sounds when any have been unreachable for
        downtimeThreshold seconds continuously.  Reachability is tested with
        ping (covers both DNS resolution and ICMP in a single probe).
      '';
    };

    downtimeThreshold = lib.mkOption {
      type = lib.types.ints.positive;
      default = 300;
      example = 60;
      description = ''
        Seconds of continuous total downtime across all monitored hosts before
        the alert starts.
      '';
    };

    checkInterval = lib.mkOption {
      type = lib.types.ints.positive;
      default = 15;
      example = 30;
      description = ''
        Seconds between reachability polls.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      alert-me-locally
      host-connectivity-monitor
    ];

    launchd.user.agents.host-connectivity-monitor = {
      path = [ config.environment.systemPath ];

      serviceConfig = {
        Label = "local.host-connectivity-monitor";
        ProgramArguments = [
          "${host-connectivity-monitor}/bin/host-connectivity-monitor"
        ]
        ++ cfg.hosts;
        EnvironmentVariables = {
          DOWNTIME_THRESHOLD = toString cfg.downtimeThreshold;
          CHECK_INTERVAL = toString cfg.checkInterval;
        };
        # Without this, macOS defaults to "Background" which enables App Nap
        # and I/O throttling.  That causes sox to block indefinitely on the
        # CoreAudio device when the screen is locked, effectively killing
        # audio output until the session resumes.
        ProcessType = "Interactive";
        RunAtLoad = true;
        KeepAlive = true;
        StandardOutPath = "/tmp/host-connectivity-monitor.log";
        StandardErrorPath = "/tmp/host-connectivity-monitor.log";
      };
    };
  };
}
