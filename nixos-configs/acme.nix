################################################################################
# ACME certificate configuration for silicon using Porkbun DNS-01 challenge.
#
# DNS-01 requires no inbound port 80; all validation goes through the Porkbun
# API.  The credential secrets are kept in agenix and injected at activation
# time.
################################################################################
{ config, ... }:
{
  age.secrets.acme-porkbun-api-key = {
    rekeyFile = ../secrets/acme-porkbun-api-key.age;
  };
  age.secrets.acme-porkbun-secret-key = {
    rekeyFile = ../secrets/acme-porkbun-secret-key.age;
  };

  security.acme = {
    acceptTerms = true;
    defaults = {
      email = "logustus+acme@gmail.com";
      dnsProvider = "porkbun";
      credentialFiles = {
        "PORKBUN_API_KEY_FILE" = config.age.secrets.acme-porkbun-api-key.path;
        "PORKBUN_SECRET_API_KEY_FILE" = config.age.secrets.acme-porkbun-secret-key.path;
      };
    };
  };
}
