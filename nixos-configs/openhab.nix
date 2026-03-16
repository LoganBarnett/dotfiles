################################################################################
# OpenHab handles controlling and automating small household devices (IoT).  It
# s a competitor to Home Assistant, and the largest pull for me is that OpenHab
# uses configuration files it wishes to keep documented, as opposed to Home
# Assistant's recent declaration of abandoning a file based configuration in
# favor of click-ops.
################################################################################
{ config, lib, flake-inputs, pkgs, system, ... }: {
  imports = [
    ../nixos-modules/https-module.nix
    # This doesn't actually work, because this module is already "imported".
    # So it has to be included in the `modules` list for the host.  Alas.
    flake-inputs.openhab-flake.nixosModules.${system}.openhab
    ../nixos-modules/authentik-oidc-client-proxy.nix
  ];
  # This has to perfectly match what is emitted from
  # ../nixos-modules/authentik.nix in order to be the same shared secret.
  age.secrets.authentik-openhab-client-secret = {
    generator.script = "hex";
    settings.length = 60;
  };
  nixpkgs.overlays = [ flake-inputs.openhab-flake.overlays.default ];
  services.oidc-proxy.fqdns."openhab.proton" = {
    issuerUrl       = "https://authentik.proton/application/o/openhab/";
    clientId        = "openhab-proxy";
    clientSecretName= "authentik-openhab-client-secret";
    internalPort    = config.services.openhab.ports.http;
    proxyPort       = 4181;        # if running multiple, give each a unique port
  };
  services.openhab = {
    enable = true;
    settings = [
      {
        name = "config/services/runtime.cfg";
        contents = {
          "org.openhab.authentication:trustHeader" = true;
          "org.openhab.authentication:headerName" = "X-Forwarded-User";
        };
      }
    ];
  };
  services.https.fqdns."openhab.proton" = {
    enable = true;
    internalPort = config.services.openhab.ports.http;
  };
}
