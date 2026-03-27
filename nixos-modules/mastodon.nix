################################################################################
# Multi-instance Mastodon module.  Each instance is keyed by a short name
# (used to namespace systemd units, directories, users, and Redis servers)
# and carries a mandatory localDomain option that sets the federation identity.
#
# Derived from nixpkgs nixos/modules/services/web-apps/mastodon.nix; all
# per-instance resources are suffixed with the instance name so multiple
# instances may coexist on one host.
################################################################################
{
  lib,
  pkgs,
  config,
  ...
}:

let
  # Convert a lowercase hexadecimal string to an integer.
  hexToInt =
    let
      digit =
        c:
        {
          "0" = 0;
          "1" = 1;
          "2" = 2;
          "3" = 3;
          "4" = 4;
          "5" = 5;
          "6" = 6;
          "7" = 7;
          "8" = 8;
          "9" = 9;
          "a" = 10;
          "b" = 11;
          "c" = 12;
          "d" = 13;
          "e" = 14;
          "f" = 15;
        }
        .${c};
    in
    s: lib.foldl' (acc: c: acc * 16 + digit c) 0 (lib.stringToCharacters s);

  # Derive a deterministic TCP port base from an instance name.  Takes the
  # first three hex digits of the SHA-256 hash (0-4095) as an offset into the
  # IANA dynamic/private port range, spaced by two so that web and sidekiq
  # ports sit adjacent without overlap.  The maximum derived port is 57342,
  # well within the 65535 ceiling.
  instanceBasePort =
    instanceName:
    let
      hash = builtins.hashString "sha256" instanceName;
      offset = hexToInt (lib.substring 0 3 hash);
    in
    49152 + offset * 2;

  # Generate the full NixOS config contribution for one Mastodon instance.
  makeInstance =
    name: cfg:
    let
      redisActuallyCreateLocally =
        cfg.redis.createLocally
        && (cfg.redis.host == "127.0.0.1" || cfg.redis.enableUnixSocket);
      databaseActuallyCreateLocally =
        cfg.database.createLocally && cfg.database.host == "/run/postgresql";

      env = {
        RAILS_ENV = "production";
        NODE_ENV = "production";

        BOOTSNAP_CACHE_DIR = "/var/cache/mastodon-${name}/precompile";
        LD_PRELOAD = "${pkgs.jemalloc}/lib/libjemalloc.so";

        WEB_CONCURRENCY = toString cfg.webProcesses;
        MAX_THREADS = toString cfg.webThreads;

        DB_USER = cfg.database.user;
        DB_HOST = cfg.database.host;
        DB_NAME = cfg.database.name;
        LOCAL_DOMAIN = cfg.localDomain;
        SMTP_SERVER = cfg.smtp.host;
        SMTP_PORT = toString cfg.smtp.port;
        SMTP_FROM_ADDRESS = cfg.smtp.fromAddress;
        PAPERCLIP_ROOT_PATH = "/var/lib/mastodon-${name}/public-system";
        PAPERCLIP_ROOT_URL = "/system";
        ES_ENABLED = if cfg.elasticsearch.host != null then "true" else "false";
        TRUSTED_PROXY_IP = cfg.trustedProxy;
      }
      // lib.optionalAttrs (cfg.redis.host != null) { REDIS_HOST = cfg.redis.host; }
      // lib.optionalAttrs (cfg.redis.port != null) {
        REDIS_PORT = toString cfg.redis.port;
      }
      // lib.optionalAttrs (cfg.redis.createLocally && cfg.redis.enableUnixSocket) {
        # Compute the socket path directly rather than reading it back from
        # config.services.redis.servers, which would create a dependency cycle
        # in the multi-instance lib.mkMerge evaluation.  The NixOS redis module
        # derives the path as /run/redis-${name}/redis.sock (redisName prefixes
        # "redis-"), so for our server "mastodon-${name}" the path is constant.
        REDIS_URL = "unix:///run/redis-mastodon-${name}/redis.sock";
      }
      //
        lib.optionalAttrs
          (cfg.database.host != "/run/postgresql" && cfg.database.port != null)
          {
            DB_PORT = toString cfg.database.port;
          }
      // lib.optionalAttrs cfg.smtp.authenticate { SMTP_LOGIN = cfg.smtp.user; }
      // lib.optionalAttrs (cfg.elasticsearch.host != null) {
        ES_HOST = cfg.elasticsearch.host;
      }
      // lib.optionalAttrs (cfg.elasticsearch.host != null) {
        ES_PORT = toString cfg.elasticsearch.port;
      }
      //
        lib.optionalAttrs
          (cfg.elasticsearch.host != null && cfg.elasticsearch.prefix != null)
          {
            ES_PREFIX = cfg.elasticsearch.prefix;
          }
      // lib.optionalAttrs (cfg.elasticsearch.host != null) {
        ES_PRESET = cfg.elasticsearch.preset;
      }
      // lib.optionalAttrs (cfg.elasticsearch.user != null) {
        ES_USER = cfg.elasticsearch.user;
      }
      // cfg.extraConfig;

      systemCallsList = [
        "@cpu-emulation"
        "@debug"
        "@keyring"
        "@ipc"
        "@mount"
        "@obsolete"
        "@privileged"
        "@setuid"
      ];

      # Common systemd service hardening applied to every Mastodon unit.
      cfgService = {
        User = cfg.user;
        Group = cfg.group;
        WorkingDirectory = cfg.package;
        CacheDirectory = "mastodon-${name}";
        CacheDirectoryMode = "0750";
        StateDirectory = "mastodon-${name}";
        StateDirectoryMode = "0750";
        LogsDirectory = "mastodon-${name}";
        LogsDirectoryMode = "0750";
        ProcSubset = "pid";
        ProtectProc = "invisible";
        UMask = "0027";
        CapabilityBoundingSet = "";
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        PrivateDevices = true;
        PrivateUsers = true;
        ProtectClock = true;
        ProtectHostname = true;
        ProtectKernelLogs = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        ProtectControlGroups = true;
        RestrictAddressFamilies = [
          "AF_UNIX"
          "AF_INET"
          "AF_INET6"
          "AF_NETLINK"
        ];
        RestrictNamespaces = true;
        LockPersonality = true;
        MemoryDenyWriteExecute = false;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        RemoveIPC = true;
        PrivateMounts = true;
        SystemCallArchitectures = "native";
      };

      secretsEnvFile = "/var/lib/mastodon-${name}/.secrets_env";

      commonUnits =
        lib.optional redisActuallyCreateLocally "redis-mastodon-${name}.service"
        ++ lib.optional databaseActuallyCreateLocally "postgresql.target"
        ++ lib.optional cfg.automaticMigrations "mastodon-${name}-init-db.service";

      envFile = pkgs.writeText "mastodon-${name}.env" (
        lib.concatMapStrings (s: s + "\n") (
          lib.concatLists (
            lib.mapAttrsToList (
              envName: value: lib.optional (value != null) ''${envName}="${toString value}"''
            ) env
          )
        )
      );

      mastodonTootctl =
        let
          sourceExtraEnv = lib.concatMapStrings (p: "source ${p}\n") cfg.extraEnvFiles;
        in
        pkgs.writeShellScriptBin "mastodon-${name}-tootctl" ''
          set -a
          export RAILS_ROOT="${cfg.package}"
          source "${envFile}"
          source ${secretsEnvFile}
          ${sourceExtraEnv}

          sudo=exec
          if [[ "$USER" != ${cfg.user} ]]; then
            sudo='exec /run/wrappers/bin/sudo -u ${cfg.user} --preserve-env'
          fi
          $sudo ${cfg.package}/bin/tootctl "$@"
        '';

      sidekiqUnits = lib.attrsets.mapAttrs' (
        procName: processCfg:
        lib.nameValuePair "mastodon-${name}-sidekiq-${procName}" (
          let
            jobClassArgs = toString (map (c: "-q ${c}") processCfg.jobClasses);
            jobClassLabel = toString ([ "" ] ++ processCfg.jobClasses);
            threads = toString (
              if processCfg.threads == null then cfg.sidekiqThreads else processCfg.threads
            );
          in
          {
            after = [
              "network.target"
              "mastodon-${name}-init-dirs.service"
            ]
            ++ commonUnits;
            requires = [ "mastodon-${name}-init-dirs.service" ] ++ commonUnits;
            description = "Mastodon ${name} sidekiq${jobClassLabel}";
            wantedBy = [ "mastodon-${name}.target" ];
            environment = env // {
              PORT = toString cfg.sidekiqPort;
              DB_POOL = threads;
            };
            serviceConfig = {
              ExecStart = "${cfg.package}/bin/sidekiq ${jobClassArgs} -c ${threads} -r ${cfg.package}";
              Restart = "always";
              RestartSec = 20;
              EnvironmentFile = [ secretsEnvFile ] ++ cfg.extraEnvFiles;
              WorkingDirectory = cfg.package;
              LimitNOFILE = "1024000";
              SystemCallFilter = [
                ("~" + lib.concatStringsSep " " systemCallsList)
                "@chown"
                "pipe"
                "pipe2"
              ];
            }
            // cfgService;
            path = with pkgs; [
              ffmpeg-headless
              file
            ];
          }
        )
      ) cfg.sidekiqProcesses;

      streamingUnits = builtins.listToAttrs (
        map (i: {
          name = "mastodon-${name}-streaming-${toString i}";
          value = {
            after = [
              "network.target"
              "mastodon-${name}-init-dirs.service"
            ]
            ++ commonUnits;
            requires = [ "mastodon-${name}-init-dirs.service" ] ++ commonUnits;
            wantedBy = [
              "mastodon-${name}.target"
              "mastodon-${name}-streaming.target"
            ];
            description = "Mastodon ${name} streaming ${toString i}";
            environment = env // {
              SOCKET = "/run/mastodon-${name}-streaming/streaming-${toString i}.socket";
            };
            serviceConfig = {
              ExecStart = "${cfg.package}/run-streaming.sh";
              Restart = "always";
              RestartSec = 20;
              EnvironmentFile = [ secretsEnvFile ] ++ cfg.extraEnvFiles;
              WorkingDirectory = cfg.package;
              RuntimeDirectory = "mastodon-${name}-streaming";
              RuntimeDirectoryMode = "0750";
              SystemCallFilter = [
                (
                  "~"
                  + lib.concatStringsSep " " (
                    systemCallsList
                    ++ [
                      "@memlock"
                      "@resources"
                    ]
                  )
                )
                "pipe"
                "pipe2"
              ];
            }
            // cfgService;
          };
        }) (lib.range 1 cfg.streamingProcesses)
      );
    in
    {
      assertions = [
        {
          assertion =
            !redisActuallyCreateLocally
            -> (cfg.redis.host != "127.0.0.1" && cfg.redis.port != null);
          message = ''
            services.mastodon.instances.${name}.redis.host and .redis.port must be set
            when .redis.createLocally is false.
          '';
        }
        {
          assertion =
            redisActuallyCreateLocally
            -> (
              !cfg.redis.enableUnixSocket
              || (cfg.redis.host == null && cfg.redis.port == null)
            );
          message = ''
            services.mastodon.instances.${name}.redis.host and .redis.port must be null
            when .redis.enableUnixSocket is true.
          '';
        }
        {
          assertion =
            redisActuallyCreateLocally
            -> (!cfg.redis.enableUnixSocket || cfg.redis.passwordFile == null);
          message = ''
            services.mastodon.instances.${name}.redis.passwordFile cannot be used
            together with .redis.enableUnixSocket.
          '';
        }
        {
          assertion =
            databaseActuallyCreateLocally
            -> (cfg.user == cfg.database.user && cfg.database.user == cfg.database.name);
          message = ''
            For local PostgreSQL peer authentication, services.mastodon.instances.${name}.user,
            .database.user, and .database.name must all be equal.
          '';
        }
        {
          assertion =
            !databaseActuallyCreateLocally -> (cfg.database.host != "/run/postgresql");
          message = ''
            services.mastodon.instances.${name}.database.host must be set when
            .database.createLocally is false.
          '';
        }
        {
          assertion = cfg.smtp.authenticate -> (cfg.smtp.user != null);
          message = ''
            services.mastodon.instances.${name}.smtp.user must be set when
            .smtp.authenticate is true.
          '';
        }
        {
          assertion = cfg.smtp.authenticate -> (cfg.smtp.passwordFile != null);
          message = ''
            services.mastodon.instances.${name}.smtp.passwordFile must be set when
            .smtp.authenticate is true.
          '';
        }
        {
          assertion =
            1 == (lib.count (x: x) (
              lib.mapAttrsToList (
                _: v: builtins.elem "scheduler" v.jobClasses || v.jobClasses == [ ]
              ) cfg.sidekiqProcesses
            ));
          message = ''
            Exactly one entry in services.mastodon.instances.${name}.sidekiqProcesses
            must handle the "scheduler" job class (or have an empty jobClasses list).
          '';
        }
        {
          assertion =
            databaseActuallyCreateLocally
            -> lib.versionAtLeast config.services.postgresql.finalPackage.version "14";
          message = "Mastodon requires PostgreSQL 14 or later.";
        }
      ];

      environment.systemPackages = [ mastodonTootctl ];

      systemd.targets."mastodon-${name}" = {
        description = "Target for all Mastodon ${name} services";
        wantedBy = [ "multi-user.target" ];
        # Soft dependency: wait for LDAP accounts to be provisioned so SMTP
        # auth works on first boot, but don't fail if the reconciler errors.
        after = [
          "network.target"
          "ldap-reconciler.service"
        ];
        wants = [ "ldap-reconciler.service" ];
      };

      systemd.targets."mastodon-${name}-streaming" = {
        description = "Target for all Mastodon ${name} streaming services";
        wantedBy = [
          "multi-user.target"
          "mastodon-${name}.target"
        ];
        after = [ "network.target" ];
      };

      systemd.services = lib.mkMerge [
        {
          "mastodon-${name}-init-dirs" = {
            script = ''
              umask 077

              if ! test -d /var/cache/mastodon-${name}/precompile; then
                ${cfg.package}/bin/bundle exec bootsnap precompile \
                  --gemfile ${cfg.package}/app ${cfg.package}/lib
              fi
              if ! test -f ${cfg.activeRecordEncryptionDeterministicKeyFile}; then
                mkdir -p $(dirname ${cfg.activeRecordEncryptionDeterministicKeyFile})
                bin/rails db:encryption:init \
                  | grep --only-matching "ACTIVE_RECORD_ENCRYPTION_DETERMINISTIC_KEY=[^ ]\+" \
                  | sed 's/^ACTIVE_RECORD_ENCRYPTION_DETERMINISTIC_KEY=//' \
                  > ${cfg.activeRecordEncryptionDeterministicKeyFile}
              fi
              if ! test -f ${cfg.activeRecordEncryptionKeyDerivationSaltFile}; then
                mkdir -p $(dirname ${cfg.activeRecordEncryptionKeyDerivationSaltFile})
                bin/rails db:encryption:init \
                  | grep --only-matching "ACTIVE_RECORD_ENCRYPTION_KEY_DERIVATION_SALT=[^ ]\+" \
                  | sed 's/^ACTIVE_RECORD_ENCRYPTION_KEY_DERIVATION_SALT=//' \
                  > ${cfg.activeRecordEncryptionKeyDerivationSaltFile}
              fi
              if ! test -f ${cfg.activeRecordEncryptionPrimaryKeyFile}; then
                mkdir -p $(dirname ${cfg.activeRecordEncryptionPrimaryKeyFile})
                bin/rails db:encryption:init \
                  | grep --only-matching "ACTIVE_RECORD_ENCRYPTION_PRIMARY_KEY=[^ ]\+" \
                  | sed 's/^ACTIVE_RECORD_ENCRYPTION_PRIMARY_KEY=//' \
                  > ${cfg.activeRecordEncryptionPrimaryKeyFile}
              fi
              if ! test -f ${cfg.secretKeyBaseFile}; then
                mkdir -p $(dirname ${cfg.secretKeyBaseFile})
                bin/bundle exec rails secret > ${cfg.secretKeyBaseFile}
              fi
              if ! test -f ${cfg.vapidPrivateKeyFile}; then
                mkdir -p $(dirname ${cfg.vapidPrivateKeyFile}) $(dirname ${cfg.vapidPublicKeyFile})
                keypair=$(bin/rake webpush:generate_keys)
                echo $keypair \
                  | grep --only-matching "Private -> [^ ]\+" \
                  | sed 's/^Private -> //' \
                  > ${cfg.vapidPrivateKeyFile}
                echo $keypair \
                  | grep --only-matching "Public -> [^ ]\+" \
                  | sed 's/^Public -> //' \
                  > ${cfg.vapidPublicKeyFile}
              fi

              cat > ${secretsEnvFile} <<EOF
              ACTIVE_RECORD_ENCRYPTION_DETERMINISTIC_KEY="$(cat ${cfg.activeRecordEncryptionDeterministicKeyFile})"
              ACTIVE_RECORD_ENCRYPTION_KEY_DERIVATION_SALT="$(cat ${cfg.activeRecordEncryptionKeyDerivationSaltFile})"
              ACTIVE_RECORD_ENCRYPTION_PRIMARY_KEY="$(cat ${cfg.activeRecordEncryptionPrimaryKeyFile})"
              SECRET_KEY_BASE="$(cat ${cfg.secretKeyBaseFile})"
              VAPID_PRIVATE_KEY="$(cat ${cfg.vapidPrivateKeyFile})"
              VAPID_PUBLIC_KEY="$(cat ${cfg.vapidPublicKeyFile})"
            ''
            + lib.optionalString (cfg.redis.passwordFile != null) ''
              REDIS_PASSWORD="$(cat ${cfg.redis.passwordFile})"
            ''
            + lib.optionalString (cfg.database.passwordFile != null) ''
              DB_PASS="$(cat ${cfg.database.passwordFile})"
            ''
            + lib.optionalString cfg.smtp.authenticate ''
              SMTP_PASSWORD="$(cat ${cfg.smtp.passwordFile})"
            ''
            + lib.optionalString (cfg.elasticsearch.passwordFile != null) ''
              ES_PASS="$(cat ${cfg.elasticsearch.passwordFile})"
            ''
            + ''
              EOF
            '';
            environment = env;
            serviceConfig = {
              Type = "oneshot";
              SyslogIdentifier = "mastodon-${name}-init-dirs";
              SystemCallFilter = [
                ("~" + lib.concatStringsSep " " (systemCallsList ++ [ "@resources" ]))
                "@chown"
                "pipe"
                "pipe2"
              ];
            }
            // cfgService;
            after = [ "network.target" ];
          };

          "mastodon-${name}-init-settings" = {
            description = "Mastodon ${name} settings initialization";
            environment = env;
            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;
              EnvironmentFile = [ secretsEnvFile ] ++ cfg.extraEnvFiles;
              WorkingDirectory = cfg.package;
            }
            // cfgService;
            script = ''
              ${cfg.package}/bin/tootctl settings registrations close
            '';
            after = [
              "network.target"
              "mastodon-${name}-init-dirs.service"
            ]
            ++ commonUnits;
            requires = [ "mastodon-${name}-init-dirs.service" ] ++ commonUnits;
            wantedBy = [ "mastodon-${name}.target" ];
          };

          "mastodon-${name}-web" = {
            after = [
              "network.target"
              "mastodon-${name}-init-dirs.service"
            ]
            ++ commonUnits;
            requires = [ "mastodon-${name}-init-dirs.service" ] ++ commonUnits;
            wantedBy = [ "mastodon-${name}.target" ];
            description = "Mastodon ${name} web";
            environment =
              env
              // (
                if cfg.enableUnixSocket then
                  { SOCKET = "/run/mastodon-${name}-web/web.socket"; }
                else
                  { PORT = toString cfg.webPort; }
              );
            serviceConfig = {
              ExecStart = "${cfg.package}/bin/puma -C config/puma.rb";
              Restart = "always";
              RestartSec = 20;
              EnvironmentFile = [ secretsEnvFile ] ++ cfg.extraEnvFiles;
              WorkingDirectory = cfg.package;
              RuntimeDirectory = "mastodon-${name}-web";
              RuntimeDirectoryMode = "0750";
              SystemCallFilter = [
                ("~" + lib.concatStringsSep " " systemCallsList)
                "@chown"
                "pipe"
                "pipe2"
              ];
            }
            // cfgService;
            path = with pkgs; [
              ffmpeg-headless
              file
            ];
          };
        }
        (lib.mkIf cfg.automaticMigrations {
          "mastodon-${name}-init-db" = {
            script =
              lib.optionalString (!databaseActuallyCreateLocally) ''
                umask 077
                export PGPASSWORD="$(cat '${cfg.database.passwordFile}')"
              ''
              + ''
                result="$(psql -t --csv -c \
                    "select count(*) from pg_class c \
                    join pg_namespace s on s.oid = c.relnamespace \
                    where s.nspname not in ('pg_catalog', 'pg_toast', 'information_schema') \
                    and s.nspname not like 'pg_temp%';")" || error_code=$?
                if [ "''${error_code:-0}" -ne 0 ]; then
                  echo "Failure checking if database is seeded. psql gave exit code $error_code"
                  exit "$error_code"
                fi
                if [ "$result" -eq 0 ]; then
                  echo "Seeding database"
                  SAFETY_ASSURED=1 rails db:schema:load
                  rails db:seed
                else
                  echo "Migrating database (this might be a noop)"
                  rails db:migrate
                fi
              ''
              + lib.optionalString (!databaseActuallyCreateLocally) ''
                unset PGPASSWORD
              '';
            path = [
              cfg.package
              (
                if databaseActuallyCreateLocally then
                  config.services.postgresql.package
                else
                  pkgs.postgresql
              )
            ];
            environment =
              env
              // lib.optionalAttrs (!databaseActuallyCreateLocally) {
                PGHOST = cfg.database.host;
                PGPORT = toString cfg.database.port;
                PGDATABASE = cfg.database.name;
                PGUSER = cfg.database.user;
              };
            serviceConfig = {
              Type = "oneshot";
              EnvironmentFile = [ secretsEnvFile ] ++ cfg.extraEnvFiles;
              WorkingDirectory = cfg.package;
              SystemCallFilter = [
                ("~" + lib.concatStringsSep " " (systemCallsList ++ [ "@resources" ]))
                "@chown"
                "pipe"
                "pipe2"
              ];
            }
            // cfgService;
            after = [
              "network.target"
              "mastodon-${name}-init-dirs.service"
            ]
            ++ lib.optional databaseActuallyCreateLocally "postgresql.target";
            requires = [
              "mastodon-${name}-init-dirs.service"
            ]
            ++ lib.optional databaseActuallyCreateLocally "postgresql.target";
          };
        })
        (lib.mkIf cfg.mediaAutoRemove.enable {
          "mastodon-${name}-media-auto-remove" = {
            description = "Mastodon ${name} media auto remove";
            environment = env;
            serviceConfig = {
              Type = "oneshot";
              EnvironmentFile = [ secretsEnvFile ] ++ cfg.extraEnvFiles;
            }
            // cfgService;
            script =
              let
                olderThanDays = toString cfg.mediaAutoRemove.olderThanDays;
              in
              ''
                ${cfg.package}/bin/tootctl media remove --days=${olderThanDays}
                ${cfg.package}/bin/tootctl preview_cards remove --days=${olderThanDays}
              '';
            startAt = cfg.mediaAutoRemove.startAt;
          };
        })
        sidekiqUnits
        streamingUnits
      ];

      services.nginx = lib.mkIf cfg.configureNginx {
        enable = true;
        # Required for redirections to work correctly.
        recommendedProxySettings = true;
        virtualHosts."${cfg.localDomain}" = {
          root = "${cfg.package}/public/";
          forceSSL = lib.mkDefault true;
          enableACME = lib.mkDefault true;

          locations."/system/".alias = "/var/lib/mastodon-${name}/public-system/";

          locations."/" = {
            tryFiles = "$uri @proxy";
          };

          locations."@proxy" = {
            proxyPass =
              if cfg.enableUnixSocket then
                "http://unix:/run/mastodon-${name}-web/web.socket"
              else
                "http://127.0.0.1:${toString cfg.webPort}";
            proxyWebsockets = true;
          };

          locations."/api/v1/streaming" = {
            proxyPass = "http://mastodon-${name}-streaming";
            proxyWebsockets = true;
          };
        };
        upstreams."mastodon-${name}-streaming" = {
          extraConfig = "least_conn;";
          servers = builtins.listToAttrs (
            map (i: {
              name = "unix:/run/mastodon-${name}-streaming/streaming-${toString i}.socket";
              value = { };
            }) (lib.range 1 cfg.streamingProcesses)
          );
        };
      };

      services.postfix =
        lib.mkIf (cfg.smtp.createLocally && cfg.smtp.host == "127.0.0.1")
          {
            enable = true;
            settings.main.myhostname = lib.mkDefault cfg.localDomain;
          };

      services.redis.servers."mastodon-${name}" =
        lib.mkIf redisActuallyCreateLocally
          (
            lib.mkMerge [
              { enable = true; }
              (lib.mkIf (!cfg.redis.enableUnixSocket) { port = cfg.redis.port; })
            ]
          );

      services.postgresql = lib.mkIf databaseActuallyCreateLocally {
        enable = true;
        ensureUsers = [
          {
            name = cfg.database.name;
            ensureDBOwnership = true;
          }
        ];
        ensureDatabases = [ cfg.database.name ];
      };

      users.users = lib.mkMerge [
        (lib.mkIf (cfg.user == "mastodon-${name}") {
          "mastodon-${name}" = {
            isSystemUser = true;
            home = cfg.package;
            inherit (cfg) group;
          };
        })
        (lib.attrsets.setAttrByPath [ cfg.user "packages" ] [ cfg.package ])
        (lib.mkIf (cfg.redis.createLocally && cfg.redis.enableUnixSocket) {
          ${cfg.user}.extraGroups = [ "redis-mastodon-${name}" ];
        })
      ];

      users.groups.${cfg.group}.members =
        lib.optional cfg.configureNginx config.services.nginx.user;
    };

  # Option declarations for a single Mastodon instance, used as a submodule.
  instanceOptions =
    { name, config, ... }:
    {
      options = {
        enable = lib.mkEnableOption "Mastodon federated social network instance ${name}";

        configureNginx = lib.mkOption {
          description = ''
            Configure nginx as a reverse proxy for this Mastodon instance.
            When disabled, wire the following paths manually:

            `/`                  → `''${package}/public` (static files, then proxy)
            `/system/`           → `/var/lib/mastodon-${name}/public-system/`
            `/@proxy`            → web socket or TCP port (websockets required)
            `/api/v1/streaming`  → streaming upstream (websockets required)
          '';
          type = lib.types.bool;
          default = false;
        };

        user = lib.mkOption {
          description = ''
            System user that runs Mastodon.  Defaults to `mastodon-''${name}`,
            which is created automatically.  Set to an existing user name to
            manage the account elsewhere.
          '';
          type = lib.types.str;
          default = "mastodon-${name}";
        };

        group = lib.mkOption {
          description = "System group that runs Mastodon.";
          type = lib.types.str;
          default = "mastodon-${name}";
        };

        localDomain = lib.mkOption {
          description = ''
            The domain that forms the identity of this Mastodon instance.
            User handles take the form `@user@''${localDomain}`.  This value
            is permanent — changing it after federation has begun will break
            remote relationships.
          '';
          type = lib.types.str;
          example = "social.example.org";
        };

        streamingProcesses = lib.mkOption {
          description = ''
            Number of mastodon-streaming worker processes.  The recommended
            value is the number of CPU cores minus one.
          '';
          type = lib.types.ints.positive;
          example = 3;
        };

        webPort = lib.mkOption {
          description = "TCP port for the mastodon-web (Puma) process.  Unused when enableUnixSocket is true.  Defaults to a value derived deterministically from the instance name.";
          type = lib.types.port;
          default = instanceBasePort name;
        };

        webProcesses = lib.mkOption {
          description = "Number of Puma worker processes.";
          type = lib.types.int;
          default = 2;
        };

        webThreads = lib.mkOption {
          description = "Threads per Puma worker process.";
          type = lib.types.int;
          default = 5;
        };

        sidekiqPort = lib.mkOption {
          description = "TCP port for Sidekiq processes.  Defaults to webPort + 1.";
          type = lib.types.port;
          default = instanceBasePort name + 1;
        };

        sidekiqThreads = lib.mkOption {
          description = ''
            Default thread count for Sidekiq processes.  Used for any process
            whose per-process `threads` is null.
          '';
          type = lib.types.int;
          default = 25;
        };

        sidekiqProcesses = lib.mkOption {
          description = ''
            Sidekiq process definitions.  Each key becomes part of the systemd
            unit name.  Read the
            [upstream scaling docs](https://docs.joinmastodon.org/admin/scaling/#sidekiq)
            before changing this.
          '';
          type = lib.types.attrsOf (
            lib.types.submodule {
              options = {
                jobClasses = lib.mkOption {
                  type = lib.types.listOf (
                    lib.types.enum [
                      "default"
                      "fasp"
                      "push"
                      "pull"
                      "mailers"
                      "scheduler"
                      "ingress"
                    ]
                  );
                  description = ''
                    Job classes this process handles.  An empty list means
                    the process handles all classes including `scheduler`.
                    Only one process across all instances may own `scheduler`.
                  '';
                };
                threads = lib.mkOption {
                  type = lib.types.nullOr lib.types.int;
                  description = "Thread count override.  Null inherits sidekiqThreads.";
                };
              };
            }
          );
          default = {
            all = {
              jobClasses = [ ];
              threads = null;
            };
          };
          example = {
            ingress = {
              jobClasses = [ "ingress" ];
              threads = 5;
            };
            default = {
              jobClasses = [ "default" ];
              threads = 10;
            };
            push-pull = {
              jobClasses = [
                "push"
                "pull"
              ];
              threads = 5;
            };
            scheduler = {
              jobClasses = [ "scheduler" ];
              threads = null;
            };
          };
        };

        vapidPublicKeyFile = lib.mkOption {
          description = ''
            Path to the Web Push VAPID public key file.  If the file at
            `vapidPrivateKeyFile` does not exist, both key files are created
            automatically on first start.
          '';
          type = lib.types.str;
          default = "/var/lib/mastodon-${name}/secrets/vapid-public-key";
        };

        vapidPrivateKeyFile = lib.mkOption {
          description = ''
            Path to the Web Push VAPID private key file.  Created automatically
            on first start if absent.
          '';
          type = lib.types.str;
          default = "/var/lib/mastodon-${name}/secrets/vapid-private-key";
        };

        secretKeyBaseFile = lib.mkOption {
          description = ''
            Path to the Rails secret key base file.  Created automatically on
            first start if absent.
          '';
          type = lib.types.str;
          default = "/var/lib/mastodon-${name}/secrets/secret-key-base";
        };

        activeRecordEncryptionDeterministicKeyFile = lib.mkOption {
          description = "Path to the Active Record deterministic encryption key file.";
          type = lib.types.str;
          default = "/var/lib/mastodon-${name}/secrets/active-record-encryption-deterministic-key";
        };

        activeRecordEncryptionKeyDerivationSaltFile = lib.mkOption {
          description = "Path to the Active Record encryption key derivation salt file.";
          type = lib.types.str;
          default = "/var/lib/mastodon-${name}/secrets/active-record-encryption-key-derivation-salt";
        };

        activeRecordEncryptionPrimaryKeyFile = lib.mkOption {
          description = "Path to the Active Record primary encryption key file.";
          type = lib.types.str;
          default = "/var/lib/mastodon-${name}/secrets/active-record-encryption-primary-key";
        };

        trustedProxy = lib.mkOption {
          description = ''
            IP address of the reverse proxy that forwards requests to the web
            process.  Mastodon uses this for rate limiting — an incorrect value
            causes all requests to appear to originate from the proxy.
          '';
          type = lib.types.str;
          default = "127.0.0.1";
        };

        enableUnixSocket = lib.mkOption {
          description = ''
            Bind the web and streaming processes to Unix domain sockets instead
            of TCP ports.
          '';
          type = lib.types.bool;
          default = true;
        };

        redis = {
          createLocally = lib.mkOption {
            description = "Provision a local Redis server for this instance.";
            type = lib.types.bool;
            default = true;
          };

          enableUnixSocket = lib.mkOption {
            description = "Connect to Redis via a Unix socket rather than TCP.";
            type = lib.types.bool;
            default = true;
          };

          host = lib.mkOption {
            description = "Redis hostname.  Null when using a Unix socket.";
            type = lib.types.nullOr lib.types.str;
            default =
              if config.redis.createLocally && !config.redis.enableUnixSocket then
                "127.0.0.1"
              else
                null;
          };

          port = lib.mkOption {
            description = "Redis port.  Null when using a Unix socket.";
            type = lib.types.nullOr lib.types.port;
            default =
              if config.redis.createLocally && !config.redis.enableUnixSocket then
                31637
              else
                null;
          };

          passwordFile = lib.mkOption {
            description = "Path to a file containing the Redis password.";
            type = lib.types.nullOr lib.types.path;
            default = null;
          };
        };

        database = {
          createLocally = lib.mkOption {
            description = "Provision a local PostgreSQL database for this instance.";
            type = lib.types.bool;
            default = true;
          };

          host = lib.mkOption {
            description = "PostgreSQL host address or Unix socket directory.";
            type = lib.types.str;
            default = "/run/postgresql";
            example = "192.168.1.10";
          };

          port = lib.mkOption {
            description = "PostgreSQL port.  Null for local peer-auth connections.";
            type = lib.types.nullOr lib.types.port;
            default = if config.database.createLocally then null else 5432;
          };

          name = lib.mkOption {
            description = "PostgreSQL database name.";
            type = lib.types.str;
            default = "mastodon-${name}";
          };

          user = lib.mkOption {
            description = "PostgreSQL user name.";
            type = lib.types.str;
            default = "mastodon-${name}";
          };

          passwordFile = lib.mkOption {
            description = "Path to a file containing the PostgreSQL password.";
            type = lib.types.nullOr lib.types.path;
            default = null;
          };
        };

        smtp = {
          createLocally = lib.mkOption {
            description = "Configure a local Postfix instance for outbound mail.";
            type = lib.types.bool;
            default = true;
          };

          authenticate = lib.mkOption {
            description = "Authenticate to the SMTP server with user/password.";
            type = lib.types.bool;
            default = false;
          };

          host = lib.mkOption {
            description = "SMTP server hostname.";
            type = lib.types.str;
            default = "127.0.0.1";
          };

          port = lib.mkOption {
            description = "SMTP server port.";
            type = lib.types.port;
            default = 25;
          };

          fromAddress = lib.mkOption {
            description = "Envelope From address for outbound mail.";
            type = lib.types.str;
          };

          user = lib.mkOption {
            description = "SMTP login name.";
            type = lib.types.nullOr lib.types.str;
            default = null;
          };

          passwordFile = lib.mkOption {
            description = "Path to a file containing the SMTP password.";
            type = lib.types.nullOr lib.types.path;
            default = null;
          };
        };

        elasticsearch = {
          host = lib.mkOption {
            description = ''
              Elasticsearch hostname.  Full-text search is disabled when null.
            '';
            type = lib.types.nullOr lib.types.str;
            default = null;
          };

          port = lib.mkOption {
            description = "Elasticsearch port.";
            type = lib.types.port;
            default = 9200;
          };

          prefix = lib.mkOption {
            description = ''
              Index name prefix.  Useful when sharing a cluster across multiple
              Mastodon instances.
            '';
            type = lib.types.nullOr lib.types.str;
            default = null;
          };

          preset = lib.mkOption {
            description = "Elasticsearch index configuration preset.";
            type = lib.types.enum [
              "single_node_cluster"
              "small_cluster"
              "large_cluster"
            ];
            default = "single_node_cluster";
          };

          user = lib.mkOption {
            description = "Elasticsearch username for optional authentication.";
            type = lib.types.nullOr lib.types.str;
            default = null;
          };

          passwordFile = lib.mkOption {
            description = "Path to a file containing the Elasticsearch password.";
            type = lib.types.nullOr lib.types.path;
            default = null;
          };
        };

        package = lib.mkPackageOption pkgs "mastodon" { };

        extraConfig = lib.mkOption {
          description = "Extra environment variables passed to all Mastodon services.";
          type = lib.types.attrs;
          default = { };
        };

        extraEnvFiles = lib.mkOption {
          description = ''
            Additional environment files sourced by all Mastodon services.
            Useful for secrets that must not appear in the Nix store.
          '';
          type = lib.types.listOf lib.types.path;
          default = [ ];
          example = [ "/run/credentials/mastodon-social-web.service/oidc-env" ];
        };

        automaticMigrations = lib.mkOption {
          description = "Run database migrations automatically on activation.";
          type = lib.types.bool;
          default = true;
        };

        mediaAutoRemove = {
          enable = lib.mkOption {
            description = ''
              Periodically remove cached remote media and preview cards older
              than `olderThanDays` days.  Recommended by the upstream admin
              setup guide.
            '';
            type = lib.types.bool;
            default = true;
          };

          startAt = lib.mkOption {
            description = "Systemd calendar expression controlling removal frequency.";
            type = lib.types.str;
            default = "daily";
          };

          olderThanDays = lib.mkOption {
            description = "Age threshold in days for remote media removal.";
            type = lib.types.int;
            default = 30;
          };
        };
      };
    };
in
{
  options.services.mastodon.instances = lib.mkOption {
    description = ''
      Mastodon instances to run on this host, keyed by a short instance name.
      The name is used to namespace systemd units, state directories, users,
      and Redis servers, so it must be a valid identifier (letters, digits,
      hyphens).  The federation identity is set by `localDomain`, not the key.
    '';
    type = lib.types.attrsOf (lib.types.submodule instanceOptions);
    default = { };
    example = lib.literalExpression ''
      {
        social = {
          enable = true;
          localDomain = "social.example.org";
          streamingProcesses = 3;
          smtp.fromAddress = "mastodon@example.org";
        };
      }
    '';
  };

  config =
    let
      enabled = lib.filterAttrs (
        _: cfg: cfg.enable
      ) config.services.mastodon.instances;
      data = lib.mapAttrs makeInstance enabled;
      merge = f: lib.mkMerge (lib.mapAttrsToList (_: d: f d) data);
    in
    {
      assertions = lib.concatLists (lib.mapAttrsToList (_: d: d.assertions) data);
      environment.systemPackages = merge (d: d.environment.systemPackages);
      systemd.targets = merge (d: d.systemd.targets);
      systemd.services = merge (d: d.systemd.services);
      services.nginx = merge (d: d.services.nginx);
      services.postfix = merge (d: d.services.postfix);
      services.redis.servers = merge (d: d.services.redis.servers);
      services.postgresql = merge (d: d.services.postgresql);
      users.users = merge (d: d.users.users);
      users.groups = merge (d: d.users.groups);
    };
}
