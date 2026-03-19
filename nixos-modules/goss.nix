################################################################################
# Enables the goss health-check service on hosts that list "goss" in their
# facts.network.hosts monitors entry.
################################################################################
{
  config,
  facts,
  host-id,
  lib,
  ...
}:
let
  hostFacts = facts.network.hosts.${host-id};
  gossEnabled = builtins.elem "goss" (hostFacts.monitors or [ ]);
in
{
  options = { };

  config = lib.mkIf gossEnabled {
    services.goss.enable = true;
  };
}
