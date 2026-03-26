################################################################################
# ivatar: self-hosted Libravatar / Gravatar-compatible federated avatar service.
#
# Runs gunicorn behind the standard https.nix reverse proxy.  PostgreSQL is
# used for storage; peer authentication means no database password is required.
# The Django SECRET_KEY is delivered via a systemd LoadCredential secret.
#
# Initialisation (migrate + collectstatic) runs in ExecStartPre on every
# start so schema upgrades apply automatically on service restarts after a
# package upgrade.
################################################################################
{
  config,
  facts,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    types
    ;

  cfg = config.services.ivatar-host;
  ivatar = pkgs.callPackage ../derivations/ivatar/default.nix { };
  stateDir = "/var/lib/ivatar";
in
{
  imports = [ ./https.nix ];

  options.services.ivatar-host = {
    enable = mkEnableOption "ivatar Libravatar-compatible avatar service";

    port = mkOption {
      type = types.port;
      default = 8765;
      description = "Internal HTTP port Gunicorn listens on.";
    };

    fqdn = mkOption {
      type = types.str;
      default = "avatar.${facts.network.domain}";
      description = "FQDN for the HTTPS reverse proxy endpoint.";
    };

    siteName = mkOption {
      type = types.str;
      default = "Avatar";
      description = "Human-readable site name shown in the ivatar UI.";
    };

    workers = mkOption {
      type = types.ints.positive;
      default = 2;
      description = "Number of Gunicorn worker processes.";
    };

    email = {
      host = mkOption {
        type = types.str;
        default = "smtp.${facts.network.domain}";
        description = "SMTP server hostname for outbound mail.";
      };

      port = mkOption {
        type = types.port;
        default = 587;
        description = "SMTP server port.";
      };

      user = mkOption {
        type = types.str;
        default = "ivatar@${facts.network.domain}";
        description = "SMTP authentication username.";
      };

      useTls = mkOption {
        type = types.bool;
        default = true;
        description = "Enable STARTTLS for outbound mail.";
      };

      fromAddress = mkOption {
        type = types.str;
        default = "noreply@${facts.network.domain}";
        description = "From address used in mail sent by ivatar.";
      };
    };
  };

  config = mkIf cfg.enable {
    services.https.fqdns.${cfg.fqdn} = {
      internalPort = cfg.port;
    };

    services.postgresql = {
      enable = true;
      ensureDatabases = [ "ivatar" ];
      ensureUsers = [
        {
          name = "ivatar";
          ensureDBOwnership = true;
        }
      ];
    };

    # Hex secret used as Django's SECRET_KEY.
    age.secrets."ivatar-secret-key" = {
      generator.script = "hex";
      settings.length = 64;
    };

    users.users.ivatar = {
      isSystemUser = true;
      group = "ivatar";
      home = stateDir;
    };
    users.groups.ivatar = { };

    systemd.services.ivatar = {
      description = "ivatar Libravatar avatar service";
      wantedBy = [ "multi-user.target" ];
      after = [
        "network.target"
        "postgresql.service"
      ];
      requires = [ "postgresql.service" ];

      environment = {
        DJANGO_SETTINGS_MODULE = "ivatar.settings";
        # PostgreSQL connection — peer auth over Unix socket, no password.
        POSTGRESQL_DATABASE = "ivatar";
        POSTGRESQL_USER = "ivatar";
        POSTGRESQL_PASSWORD = "";
        # Empty host means Unix socket; NixOS PostgreSQL listens on
        # /run/postgresql by default.
        POSTGRESQL_HOST = "";
        # Runtime paths written into the service state directory.
        IVATAR_STATIC_ROOT = "${stateDir}/static";
        IVATAR_CACHE_DIR = "${stateDir}/cache";
        # Site identity.
        SITE_NAME = cfg.siteName;
        SECURE_BASE_URL = "https://${cfg.fqdn}/avatar/";
        BASE_URL = "https://${cfg.fqdn}/avatar/";
        # Email.
        EMAIL_BACKEND = "django.core.mail.backends.smtp.EmailBackend";
        SERVER_EMAIL = cfg.email.fromAddress;
        DEFAULT_FROM_EMAIL = cfg.email.fromAddress;
        DJANGO_EMAIL_HOST = cfg.email.host;
        DJANGO_EMAIL_PORT = toString cfg.email.port;
        DJANGO_EMAIL_HOST_USER = cfg.email.user;
        DJANGO_EMAIL_USE_TLS = if cfg.email.useTls then "1" else "0";
      };

      serviceConfig = {
        User = "ivatar";
        Group = "ivatar";
        StateDirectory = "ivatar";
        StateDirectoryMode = "0750";
        WorkingDirectory = stateDir;

        LoadCredential = [
          "secret-key:${config.age.secrets.ivatar-secret-key.path}"
        ];

        # Create sub-directories, apply pending migrations, then collect static
        # files.  All three run as the service user before Gunicorn starts.
        ExecStartPre = [
          "${pkgs.coreutils}/bin/mkdir -p ${stateDir}/static ${stateDir}/cache"
          "${ivatar}/bin/ivatar-manage migrate --noinput"
          "${ivatar}/bin/ivatar-manage collectstatic --noinput --clear"
        ];

        ExecStart = lib.concatStringsSep " " [
          "${ivatar}/bin/ivatar-server"
          "--bind 127.0.0.1:${toString cfg.port}"
          "--workers ${toString cfg.workers}"
          "ivatar.wsgi:application"
        ];

        Restart = "on-failure";
        RestartSec = "5s";
      };
    };
  };
}
