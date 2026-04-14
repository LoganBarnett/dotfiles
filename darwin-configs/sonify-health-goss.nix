################################################################################
# Goss health checks for sonify-health on darwin.
################################################################################
{
  config,
  ...
}:
let
  cfg = config.services.sonify-health;
in
{
  services.goss.checks = {
    http."http://localhost:${toString cfg.port}/healthz" = {
      status = 200;
      timeout = 5000;
    };
  };
}
