################################################################################
# Manages email-related Porkbun DNS records for meshward.com and
# logustus.com via the typed nix-hapi-porkbun module.
#
# Records managed here (per domain):
#   MX/@                    — inbound mail server
#   TXT/@                   — SPF policy
#   TXT/default._domainkey  — DKIM public key (Ed25519, from generated .pub)
#   TXT/_dmarc              — DMARC policy
#
# Records excluded via ignore expressions (jq, keyed on TYPE/name):
#   TXT/_acme-challenge*  — Let's Encrypt DNS-01 (managed by security.acme)
#   A/*                   — wildcard A (managed by dns-dynamic-ip-home/dness)
#   A/vpn                 — VPN A record (managed by dness, logustus.com only)
#   NS/*                  — apex NS records (managed by the registrar)
#
# Other Porkbun-managed records (e.g. blog CNAMEs) live in sibling files
# that contribute to the same `services.nix-hapi-porkbun.scopes`; per-key
# merging means contributions compose without losing config.
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

  cred-path = name: "/run/credentials/nix-hapi-porkbun.service/${name}";

  api-key = mkManagedFromPath (cred-path "porkbun-api-key");
  api-secret = mkManagedFromPath (cred-path "porkbun-api-secret");

  # jq expressions for records to leave alone on every reconciliation.
  # Each expression receives . = {"key": "TYPE/name", "resource_id": "TYPE/name"}.
  ignore-acme = ".key | startswith(\"TXT/_acme-challenge\")";
  ignore-wildcard-a = ".key == \"A/*\"";
  ignore-vpn-a = ".key == \"A/vpn\"";
  # NS records at the apex are managed by the registrar; never delete them.
  ignore-ns = ".key | startswith(\"NS/\")";

  # Email DNS records common to any externally-hosted domain.
  emailRecords = domain: dkimPub: {
    "MX/@" = {
      content = "mail.${domain}";
      prio = "10";
    };
    "TXT/@" = {
      content = "v=spf1 mx ~all";
    };
    "TXT/default._domainkey" = {
      content = "v=DKIM1; k=ed25519; p=${dkimPub}";
    };
    "TXT/_dmarc" = {
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
  imports = [
    flake-inputs.nix-hapi-provider-porkbun.nixosModules.default
  ];

  services.nix-hapi.enable = true;
  services.nix-hapi-porkbun = {
    enable = true;
    # Use the consumer's flake-inputs view of the package so that
    # nix-config-private's `overridePkg` (which substitutes the patched
    # build with rewritten Cargo git deps for the gitea-only environment)
    # is what actually runs.  The module's own `self.packages.…` default
    # would resolve to the un-patched build, whose Cargo.lock points
    # nix-hapi-lib at GitHub — unreachable from the nix-daemon here.
    package = flake-inputs.nix-hapi-provider-porkbun.packages.${system}.default;

    scopes."meshward.com" = {
      provider = {
        api_key = api-key;
        secret_api_key = api-secret;
      };
      ignore = [
        ignore-acme
        ignore-wildcard-a
        ignore-ns
      ];
      records = emailRecords "meshward.com" meshward-dkim-pub;
    };

    scopes."logustus.com" = {
      provider = {
        api_key = api-key;
        secret_api_key = api-secret;
      };
      ignore = [
        ignore-acme
        ignore-wildcard-a
        ignore-vpn-a
        ignore-ns
      ];
      records = emailRecords "logustus.com" logustus-dkim-pub;
    };
  };

  # Extend the generated nix-hapi-porkbun service with credential binding,
  # dependency ordering, and restart triggers.
  systemd.services.nix-hapi-porkbun = {
    after = [ "run-agenix.d.mount" ];
    wants = [ "run-agenix.d.mount" ];
    # Restart when the desired-state JSON changes (new records, updated keys).
    restartTriggers = [
      config.services.nix-hapi.jsonFiles.porkbun
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
