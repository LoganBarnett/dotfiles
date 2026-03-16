################################################################################
# Enables the Goss Prometheus exporter on darwin hosts that declare "goss" in
# their monitors list in facts.
################################################################################
{ facts, host-id, ... }:
let
  hostFacts = facts.network.hosts.${host-id};
  gossEnabled = builtins.elem "goss" (hostFacts.monitors or [ ]);
in
{
  services.goss.prometheus.enable = gossEnabled;
  services.goss.prometheus.openFirewall = gossEnabled;
}
