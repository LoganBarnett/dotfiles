################################################################################
# SFTPGo file server with SFTP, WebDAV, and web UI.  Authenticates via
# Authelia OIDC.  Media lives on the /tank/data LVM volume.
################################################################################
{
  config,
  facts,
  lib,
  pkgs,
  ...
}:
let
  service = "sftpgo";
  domain = "${service}.${facts.network.domain}";
  dataDir = "/tank/data/sftpgo";
  httpPort = 8580; # web UI + OIDC callbacks
  webdavPort = 8590; # WebDAV protocol
  sftpPort = 2223; # SFTP protocol (2222 taken by Gitea)
in
{
  networking.dnsAliases = [
    "sftpgo"
    "webdav"
  ];

  # SFTPGo is AGPL-3.0 + unfreeRedistributable (bundled GeoIP database).
  allowUnfreePackagePredicates = [
    (pkg: lib.getName pkg == "sftpgo")
  ];

  tankVolumes.volumes.${service} = {
    group = service;
    pgDatabase = service;
    backupData = true;
  };

  # Reverse proxy the web UI (admin + client + OIDC callbacks).
  services.https.fqdns."${domain}" = {
    enable = true;
    internalPort = httpPort;
  };

  # Reverse proxy WebDAV on its own subdomain so clients can mount it directly.
  services.https.fqdns."webdav.${facts.network.domain}" = {
    enable = true;
    internalPort = webdavPort;
  };

  services.sftpgo = {
    enable = true;
    dataDir = dataDir;
    settings = {
      httpd.bindings = [
        {
          port = httpPort;
          address = "127.0.0.1";
          enable_web_admin = true;
          enable_web_client = true;
          oidc = {
            client_id = "sftpgo";
            # client_secret injected at runtime via LoadCredential (see below).
            client_secret = "";
            config_url = "https://authelia.${facts.network.domain}";
            redirect_base_url = "https://${domain}";
            username_field = "preferred_username";
            scopes = [
              "openid"
              "profile"
              "email"
              "groups"
            ];
            implicit_roles = true;
          };
        }
      ];
      webdavd.bindings = [
        {
          port = webdavPort;
          address = "127.0.0.1";
        }
      ];
      sftpd.bindings = [
        {
          port = sftpPort;
          address = ""; # all interfaces (for LAN SFTP access)
        }
      ];
      ftpd.bindings = [ { port = 0; } ]; # disabled
      data_provider = {
        driver = "postgresql";
        name = service;
        host = "/run/postgresql"; # Unix socket, peer auth
        port = 0;
        username = service;
        password = "";
      };
    };
  };

  services.postgresql = {
    enable = true;
    ensureDatabases = [ service ];
    ensureUsers = [
      {
        name = service;
        ensureDBOwnership = true;
      }
    ];
  };

  # SFTPGo has no client_secret_file option.  The NixOS module generates a
  # static JSON config in the Nix store, so we cannot embed the secret there.
  # Override ExecStart with a wrapper that reads the secret via LoadCredential
  # (which systemd copies into /run/credentials/<unit>/ as the service user)
  # into SFTPGO_HTTPD__BINDINGS__0__OIDC__CLIENT_SECRET, then exec's the
  # real binary.
  # TODO: Upstream the sftpgo NixOS module to use postgresql.target when a
  # postgresql data_provider is configured, so we can drop the manual ordering.
  systemd.services.sftpgo =
    let
      after = [
        "authelia-authelia-ready.service"
        "postgresql.target"
        "run-agenix.d.mount"
        "tank-data.mount"
      ];
    in
    {
      inherit after;
      requires = after;
      serviceConfig.LoadCredential = [
        "oidc-client-secret:${config.age.secrets."sftpgo-oidc-client-secret".path}"
      ];
      serviceConfig.ExecStart = lib.mkForce (
        toString (
          pkgs.writeShellScript "sftpgo-start" ''
            export SFTPGO_HTTPD__BINDINGS__0__OIDC__CLIENT_SECRET=$(cat "$CREDENTIALS_DIRECTORY/oidc-client-secret")
            exec ${config.services.sftpgo.package}/bin/sftpgo serve
          ''
        )
      );
    };

  # Open firewall for SFTP (direct LAN access, not proxied).
  networking.firewall.allowedTCPPorts = [ sftpPort ];

  # Goss health checks.
  services.goss.checks = {
    http."https://${domain}" = {
      status = 200;
      timeout = 5000;
    };
    port."tcp6:${toString sftpPort}" = {
      listening = true;
    };
    service.sftpgo = {
      enabled = true;
      running = true;
    };
  };
}
