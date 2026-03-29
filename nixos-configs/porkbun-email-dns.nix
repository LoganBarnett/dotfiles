################################################################################
# Manages email-related DNS records for meshward.com and logustus.com via
# nix-hapi-provider-porkbun.
#
# Records managed here (per domain):
#   MX/@                    — inbound mail server
#   TXT/@                   — SPF policy
#   TXT/default._domainkey  — DKIM public key (Ed25519, from generated .pub)
#   TXT/_dmarc              — DMARC policy
#
# Records excluded via ignore patterns:
#   ^TXT/_acme-challenge  — Let's Encrypt DNS-01 (managed by security.acme)
#   ^A/\*$                — wildcard A (managed by dns-dynamic-ip-home/dness)
#   ^A/vpn$               — VPN A record (managed by dness, logustus.com only)
#
# Credentials: reuses the same Porkbun API key/secret already declared by
# acme.nix.  The nix-hapi-porkbun service binds them via LoadCredential.
################################################################################
{
  config,
  flake-inputs,
  lib,
  system,
  ...
}:
let
  inherit (flake-inputs.nix-hapi.lib) mkManagedFromPath;
  inherit (flake-inputs.nix-hapi-provider-porkbun.lib) mkPorkbunProvider mkRecord;

  cred-path = name: "/run/credentials/nix-hapi-porkbun.service/${name}";

  api-key = mkManagedFromPath (cred-path "porkbun-api-key");
  api-secret = mkManagedFromPath (cred-path "porkbun-api-secret");

  # Regex patterns for records to leave alone on every reconciliation.
  ignore-acme = "^TXT/_acme-challenge";
  ignore-wildcard-a = "^A/\\*$";
  ignore-vpn-a = "^A/vpn$";

  # Email DNS records common to any externally-hosted domain.
  emailRecords = domain: dkimPub: {
    "MX/@" = mkRecord {
      content = "mail.${domain}";
      prio = "10";
    };
    "TXT/@" = mkRecord { content = "v=spf1 mx ~all"; };
    "TXT/default._domainkey" = mkRecord {
      content = "v=DKIM1; k=ed25519; p=${dkimPub}";
    };
    "TXT/_dmarc" = mkRecord {
      content = "v=DMARC1; p=none; rua=mailto:logustus+dmarc@gmail.com";
    };
  };

  # DKIM public keys are written alongside the generated secrets by the
  # stalwart-dkim-key generator.  They are committed as plain files and safe
  # to embed at eval time.
  meshward-dkim-pub = lib.strings.removeSuffix "\n" (
    builtins.readFile ../secrets/generated/stalwart-dkim-meshward.pub
  );
  logustus-dkim-pub = lib.strings.removeSuffix "\n" (
    builtins.readFile ../secrets/generated/stalwart-dkim-logustus.pub
  );
in
{
  services.nix-hapi = {
    enable = true;
    trees.porkbun = {
      desiredState = {
        meshward-dns = mkPorkbunProvider {
          domain = "meshward.com";
          api_key = api-key;
          secret_api_key = api-secret;
          ignore = [
            ignore-acme
            ignore-wildcard-a
          ];
          records = emailRecords "meshward.com" meshward-dkim-pub;
        };
        logustus-dns = mkPorkbunProvider {
          domain = "logustus.com";
          api_key = api-key;
          secret_api_key = api-secret;
          ignore = [
            ignore-acme
            ignore-wildcard-a
            ignore-vpn-a
          ];
          records = emailRecords "logustus.com" logustus-dkim-pub;
        };
      };
      providers.porkbun =
        "${flake-inputs.nix-hapi-provider-porkbun.packages.${system}.default}"
        + "/bin/nix-hapi-provider-porkbun";
    };
  };

  # Extend the generated nix-hapi-porkbun service with credential binding,
  # dependency ordering, and restart triggers.
  systemd.services.nix-hapi-porkbun = {
    after = [ "run-agenix.d.mount" ];
    wants = [ "run-agenix.d.mount" ];
    # Restart when the desired-state JSON changes (new records, updated keys).
    restartTriggers = [
      config.environment.etc."nix-hapi/porkbun.json".source
    ];
    serviceConfig = {
      RemainAfterExit = true;
      LoadCredential = [
        "porkbun-api-key:${config.age.secrets.acme-porkbun-api-key.path}"
        "porkbun-api-secret:${config.age.secrets.acme-porkbun-secret-key.path}"
      ];
      DynamicUser = true;
      PrivateTmp = true;
      ProtectSystem = "strict";
      ProtectHome = true;
    };
  };
}
