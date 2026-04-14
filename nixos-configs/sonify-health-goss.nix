################################################################################
# Goss health checks for sonify-health.
################################################################################
{
  facts,
  host-id,
  ...
}:
let
  domain = "${host-id}-sonify.${facts.network.domain}";
in
{
  networking.dnsAliases = [ "${host-id}-sonify" ];
  services.goss.checks = {
    http."https://${domain}/healthz" = {
      status = 200;
      timeout = 5000;
    };
    service.sonify-health = {
      running = true;
    };
  };
}
