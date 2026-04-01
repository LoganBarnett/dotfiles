################################################################################
# Stalwart mail server module.
#
# Declares services.stalwart-host options and builds Stalwart's TOML
# configuration from them.  Importing this module makes the options available;
# set services.stalwart-host.enable = true (via nixos-configs/stalwart.nix) to
# activate.
#
# Listener layout:
#   25   SMTP     STARTTLS   Inbound MX
#   587  SMTP     STARTTLS   Authenticated client submission
#   993  IMAP     implicit   Mailbox access
#
# TLS: internal CA cert for mail.<internalDomain>; Let's Encrypt for external
# domains via the NixOS ACME module (DNS challenge, provider TBD).
################################################################################
{
  config,
  lib,
  ...
}:
let
  inherit (lib)
    concatMap
    map
    mkEnableOption
    mkIf
    mkMerge
    mkOption
    types
    ;
  cfg = config.services.stalwart-host;
  internalFqdn = "mail.${cfg.internalDomain}";
  # Credential name delivered via LoadCredential for the LDAP bind password.
  ldapCredential = "${cfg.ldap.serviceAccountName}-ldap-password";

in
{
  options.services.stalwart-host = {
    enable = mkEnableOption "Stalwart mail server";

    internalDomain = mkOption {
      type = types.str;
      description = ''Local-only mail domain (e.g. "proton").'';
    };

    externalDomains = mkOption {
      type = types.listOf (
        types.submodule {
          options = {
            domain = mkOption {
              type = types.str;
              description = ''External domain name (e.g. "meshward.com").'';
            };
            dkimSelector = mkOption {
              type = types.str;
              default = "default";
              description = "DKIM selector published in the domain's DNS TXT record.";
            };
            dkimSecretName = mkOption {
              type = types.str;
              description = "Name of the agenix secret holding the Ed25519 DKIM private key.";
            };
            catchAll = mkOption {
              type = types.bool;
              default = false;
              description = "Route all unmatched addresses at this domain to the owner's inbox.";
            };
          };
        }
      );
      default = [ ];
      description = "External mail domains hosted on this server.";
    };

    # Stalwart-side address aliases: external address → LDAP-backed local
    # address.  Requires LDAP mailAlias attribute support or an equivalent
    # in-memory directory override — not yet wired into settings.
    # TODO: wire once LDAP schema includes mailAlias.
    aliases = mkOption {
      type = types.attrsOf types.str;
      default = { };
      description = "Address aliases mapping external addresses to LDAP-backed local addresses.";
    };

    ldap = {
      url = mkOption {
        type = types.str;
        description = ''LDAP server URL, e.g. "ldaps://ldap.proton:636".'';
      };
      baseDn = mkOption {
        type = types.str;
        description = ''LDAP base DN, e.g. "dc=proton,dc=org".'';
      };
      # Bind DN is derived as "uid=<name>,ou=users,<baseDn>".  The plaintext
      # password secret "<name>-ldap-password" is auto-emitted by ldap-auth.nix
      # when auth.ldap.users.<name> is declared.
      serviceAccountName = mkOption {
        type = types.str;
        default = "stalwart";
        description = "Username of the LDAP service account used for directory lookups.";
      };
    };

    # Internal CA cert/key for mail.<internalDomain>.  The module auto-declares
    # the TLS leaf via tls.tls-leafs; these options just point to the resulting
    # artifacts.
    internalTls = {
      certFile = mkOption {
        type = types.str;
        description = "Absolute path to the internal CA TLS certificate file.";
      };
      keySecretName = mkOption {
        type = types.str;
        description = "Name of the agenix secret holding the TLS private key.";
      };
    };
  };

  config = mkIf cfg.enable {
    networking.dnsAliases = [ "mail" ];
    # Declare the internal TLS leaf cert.  agenix-rekey generates
    # secrets/tls-mail.<internalDomain>.key.age and the plain .crt file.
    tls.tls-leafs.${internalFqdn} = {
      fqdn = internalFqdn;
      ca = config.age.secrets.proton-ca;
    };

    services.stalwart-mail = {
      enable = true;
      settings = mkMerge (
        [
          {
            # All data types backed by a single embedded RocksDB instance.
            # Migrate to PostgreSQL later if needed.
            storage = {
              data = "rocksdb";
              blob = "rocksdb";
              fts = "rocksdb";
              lookup = "rocksdb";
              directory = "ldap";
            };

            store."rocksdb" = {
              type = "rocksdb";
              path = "/tank/data/stalwart-mail/data";
              compression = "lz4";
            };

            # LDAP directory for user authentication and address lookup.
            # Users bind with their own credentials via the re-bind flow;
            # the service account is used only for lookups.
            directory."ldap" = {
              type = "ldap";
              url = cfg.ldap.url;
              base-dn = cfg.ldap.baseDn;
              bind = {
                dn = "uid=${cfg.ldap.serviceAccountName},ou=users,${cfg.ldap.baseDn}";
                # %{file:...}% reads the credential at runtime so the password
                # never appears in the Nix store.
                secret = "%{file:/run/credentials/stalwart-mail.service/${ldapCredential}}%";
              };
              filter = {
                name = "(&(objectClass=inetOrgPerson)(uid=?))";
                email = "(&(objectClass=inetOrgPerson)(mail=?))";
                verify = "(&(objectClass=inetOrgPerson)(mail=?))";
                expand = "";
                domains = "(&(objectClass=inetOrgPerson)(uid=?))";
              };
              attributes = {
                name = "uid";
                description = [ "cn" ];
                secret = "userPassword";
                email = [ "mail" ];
                member-of = [ "memberOf" ];
              };
            };

            server = {
              hostname = internalFqdn;

              # Port 25: inbound MX.
              listener."smtp" = {
                bind = [ "0.0.0.0:25" ];
                protocol = "smtp";
                tls.implicit = false;
              };

              # Port 587: authenticated client submission only.
              listener."submission" = {
                bind = [ "0.0.0.0:587" ];
                protocol = "smtp";
                tls.implicit = false;
              };

              # Port 993: IMAP with implicit TLS for mailbox access.
              listener."imaps" = {
                bind = [ "0.0.0.0:993" ];
                protocol = "imap";
                tls.implicit = true;
              };

              tls = {
                enable = true;
                implicit = false;
                # External domain certs listed first; Stalwart uses SNI to
                # select the right one and falls back to "internal".
                certificate = (map (d: d.domain) cfg.externalDomains) ++ [ "internal" ];
              };
            };

            # Internal CA certificate for mail.<internalDomain>.
            certificate."internal" = {
              cert = "%{file:${cfg.internalTls.certFile}}%";
              private-key = "%{file:/run/credentials/stalwart-mail.service/tls-key}%";
            };

            # Allow relay only for authenticated users.
            session.rcpt.relay = [
              {
                "if" = "!is_empty(authenticated_as)";
                "then" = true;
              }
              { "else" = false; }
            ];
          }
        ]
        # Per external domain: Let's Encrypt TLS certificate + DKIM signing.
        ++ map (d: {
          # fullchain.pem is world-readable; key.pem is delivered via
          # LoadCredential so no group membership changes are needed.
          certificate.${d.domain} = {
            cert = "/var/lib/acme/${d.domain}/fullchain.pem";
            private-key = "%{file:/run/credentials/stalwart-mail.service/acme-key-${d.domain}}%";
          };

          auth.dkim.sign.${d.domain} = {
            algo = "ed25519-sha256";
            domain = d.domain;
            selector = d.dkimSelector;
            private-key = "%{file:/run/credentials/stalwart-mail.service/${d.dkimSecretName}}%";
            headers.relaxed = [
              "From"
              "To"
              "Message-ID"
              "Date"
              "Subject"
              "MIME-Version"
            ];
            canonicalization = "relaxed/relaxed";
          };
        }) cfg.externalDomains
      );
    };

    systemd.services.stalwart-mail = {
      after = [
        "ldap-reconciler.service"
        "run-agenix.d.mount"
        "tank-data.mount"
      ];
      wants = [ "ldap-reconciler.service" ];
      requires = [
        "run-agenix.d.mount"
        "tank-data.mount"
      ];
      serviceConfig = {
        # ProtectSystem=strict (set by the upstream module) makes the
        # filesystem read-only.  The tank RocksDB path is outside the
        # StateDirectory so it needs an explicit exemption.
        ReadWritePaths = [ "/tank/data/stalwart-mail" ];
        LoadCredential = [
          "${ldapCredential}:${config.age.secrets.${ldapCredential}.path}"
          "tls-key:${config.age.secrets.${cfg.internalTls.keySecretName}.path}"
        ]
        ++ concatMap (d: [
          "${d.dkimSecretName}:${config.age.secrets.${d.dkimSecretName}.path}"
          "acme-key-${d.domain}:/var/lib/acme/${d.domain}/key.pem"
        ]) cfg.externalDomains;
      };
    };

    # Mail data lives on the tank volume so it participates in the existing
    # btrfs + restic backup pipeline.  No quiesce needed — RocksDB's WAL
    # ensures every btrfs snapshot of a live instance is recoverable.
    # group = "stalwart-mail" ensures tmpfiles creates the tank directory with
    # the right ownership so the service user can write to the RocksDB path.
    tankVolumes.volumes.stalwart-mail = {
      backupData = true;
      group = "stalwart-mail";
    };

    networking.firewall.allowedTCPPorts = [
      25
      587
      993
    ];
  };
}
