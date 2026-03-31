################################################################################
# Registers a UPnP port-forward on the upstream router via miniupnpc and
# refreshes it on a 12-hour schedule so the mapping survives router reboots.
################################################################################
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.upnp-portforward;
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    types
    ;
in
{
  options.services.upnp-portforward = {
    enable = mkEnableOption "UPnP port-forward via miniupnpc";
    addr = mkOption {
      type = types.str;
      description = "Local IP address to forward external traffic to.";
    };
    externalPort = mkOption {
      type = types.port;
      description = "Port on the router's WAN interface to forward.";
    };
    localPort = mkOption {
      type = types.port;
      description = "Port on the local host to receive forwarded traffic.";
    };
    protocol = mkOption {
      type = types.str;
      default = "TCP";
      description = "Transport protocol to forward (TCP or UDP).";
    };
  };

  config = mkIf cfg.enable {
    systemd.services.upnp-portforward = {
      description = "Register UPnP port-forward with the upstream router";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      # No wantedBy here — the timer is the sole activator.  Letting
      # multi-user.target pull the service in directly races against the
      # network becoming available and causes an immediate failure on every
      # deploy/boot.
      serviceConfig = {
        Type = "oneshot";
        ExecStart =
          "${pkgs.miniupnpc}/bin/upnpc"
          + " -a ${cfg.addr} ${toString cfg.localPort}"
          + " ${toString cfg.externalPort} ${cfg.protocol}";
        RemainAfterExit = true;
      };
    };

    systemd.timers.upnp-portforward = {
      description = "Refresh UPnP port-forward every 12 hours";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        # Give the network time to settle after boot before the first run.
        OnBootSec = "2min";
        # Retry frequently during the first hour in case the router's UPnP
        # service is slow to start, then fall back to the 12-hour refresh.
        OnUnitInactiveSec = "5min";
      };
    };
  };
}
