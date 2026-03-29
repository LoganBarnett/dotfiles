################################################################################
# Enables the Goss Prometheus exporter on darwin hosts that declare "goss" in
# networking.monitors.
################################################################################
{ config, ... }:
let
  gossEnabled = builtins.elem "goss" config.networking.monitors;
in
{
  services.goss.prometheus.enable = gossEnabled;
  services.goss.prometheus.openFirewall = gossEnabled;
}
