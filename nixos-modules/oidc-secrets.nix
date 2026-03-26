################################################################################
# Emits agenix OIDC client secrets for every service in facts that uses OIDC
# authentication, on any host where either the service itself or the Authelia
# identity provider is running.
#
# Authelia needs every OIDC client secret to issue tokens.  A service host
# needs only its own secret to configure its OIDC client.  By expressing this
# logic centrally, individual service configs stay self-contained without
# manually re-declaring shared secrets.
################################################################################
{
  config,
  facts,
  lib,
  ...
}:
let
  credName = name: "${name}-oidc-client-secret";
  oidcServices = lib.filterAttrs (
    _: svc: (svc.authentication or null) == "oidc"
  ) facts.network.services;
  autheliaEnabled = config.services.authelia.instances != { };
  serviceEnabled =
    name: svc:
    let
      nixosService = svc.nixosService or name;
    in
    (config.services.${nixosService} or { }).enable or false;
in
{
  age.secrets = lib.mkMerge (
    lib.mapAttrsToList (
      name: svc:
      lib.mkIf (autheliaEnabled || serviceEnabled name svc) {
        ${credName name} = {
          generator.script = "hex";
          settings.length = 64;
        };
      }
    ) oidcServices
  );
}
