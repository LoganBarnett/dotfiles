################################################################################
# Enables the Goss Prometheus exporter on darwin hosts that declare "goss" in
# their monitors list in facts.
################################################################################
{ facts, host-id, ... }:
let
  hostFacts = facts.network.hosts.${host-id};
in
{
  services.goss.prometheus.enable = builtins.elem "goss" (
    hostFacts.monitors or [ ]
  );
}
