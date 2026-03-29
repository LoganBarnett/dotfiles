################################################################################
# Enables the goss health-check service on hosts that declare "goss" in
# networking.monitors.
################################################################################
{
  config,
  lib,
  ...
}:
{
  options = { };

  config = lib.mkIf (builtins.elem "goss" config.networking.monitors) {
    services.goss.enable = true;
  };
}
