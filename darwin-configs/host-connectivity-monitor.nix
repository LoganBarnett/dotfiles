################################################################################
# Local alert service for home network connectivity monitoring.
#
# Watches the gateway, the local DNS server (silicon), and external reference
# points (OpenDNS, Anthropic API).  IP entries detect layer-3 reachability;
# name entries also exercise DNS resolution — both must fail before the alarm
# can sound.  If all entries have been unreachable for 10 continuous minutes
# the alarm sounds — one surviving host is enough to keep it quiet.
################################################################################
{ ... }:
{
  services.hostConnectivityMonitor = {
    enable = true;
    hosts = [
      "192.168.254.254" # barnett-main gateway
      "192.168.254.9" # silicon (home DNS) — IP
      "silicon.proton" # silicon (home DNS) — name; exercises local DNS
      "208.67.222.222" # OpenDNS (external reference) — IP
      "resolver1.opendns.com" # OpenDNS — name; exercises external DNS
      "api.anthropic.com" # Anthropic/Claude API — name; CDN-backed, no stable IP
    ];
    downtimeThreshold = 600; # 10 minutes
  };
}
