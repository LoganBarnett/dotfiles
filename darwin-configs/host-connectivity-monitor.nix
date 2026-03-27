################################################################################
# Local alert service for home network connectivity monitoring.
#
# Watches the gateway, the local DNS server (silicon), and an external
# reference point (OpenDNS).  If all three have been unreachable for
# 10 continuous minutes the alarm sounds — one surviving host is enough
# to keep it quiet.
################################################################################
{ ... }:
{
  imports = [ ../darwin-modules/host-connectivity-monitor.nix ];

  services.hostConnectivityMonitor = {
    enable = true;
    hosts = [
      "192.168.254.254" # barnett-main gateway
      "192.168.254.9" # silicon (home DNS)
      "208.67.222.222" # OpenDNS (external reference)
    ];
    downtimeThreshold = 600; # 10 minutes
  };
}
