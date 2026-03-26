################################################################################
# ivatar Libravatar-compatible avatar service on silicon.
################################################################################
{ config, facts, ... }:
{
  services.ivatar-host = {
    enable = true;
    fqdn = "ivatar.${facts.network.domain}";
    oidc.enable = true;
  };

  services.https.fqdns."ivatar.${facts.network.domain}" = {
    internalPort = config.services.ivatar-host.port;
  };

  systemd.services.ivatar = {
    after = [ "run-agenix.d.mount" ];
    requires = [ "run-agenix.d.mount" ];
  };
}
