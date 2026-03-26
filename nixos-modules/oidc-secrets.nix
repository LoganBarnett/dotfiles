################################################################################
# Emits agenix OIDC client secrets for every service in facts that uses OIDC
# authentication, on any host where either the service itself or the Authelia
# identity provider is running.
#
# Authelia needs every OIDC client secret to issue tokens.  A service host
# needs only its own secret to configure its OIDC client.  By expressing this
# logic centrally, individual service configs stay self-contained without
# manually re-declaring shared secrets.
#
# When a service is running locally, its systemd unit is also given a
# restartTrigger on the secret's rekeyed .age file.  The file lives in the Nix
# store and its path changes on every rekey, so NixOS treats the unit as
# changed and restarts it — ensuring the service always picks up rotated
# secrets without a manual intervention.  Authelia handles its own triggers
# (it restarts on any client-secret change); this covers only the service-side
# units.
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

  systemd.services = lib.mkMerge (
    lib.mapAttrsToList (
      name: svc:
      let
        nixosService = svc.nixosService or name;
      in
      lib.mkIf (serviceEnabled name svc) {
        ${nixosService}.restartTriggers = [
          config.age.secrets.${credName name}.file
        ];
      }
    ) oidcServices
  );
}
