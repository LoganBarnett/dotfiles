################################################################################
#
################################################################################
{ config, host-id, ... }: {
  imports = [
    (import ../nixos-modules/https.nix {
      server-port = config.services.grafana.port;
      inherit host-id;
      fqdn = "authentik.proton";
    })
  ];
  services.authentik = {
    enable = true;
    postgresql = {
      createLocally = true;
      host = "/run/postgresql";
    };
  };
}
