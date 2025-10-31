{ config, host-id, pkgs, ... }: {
  age.secrets = {
    "${host-id}-litellm-chatgpt-api-key" = {
      rekeyFile = ../secrets/${host-id}-litellm-chatgpt-api-key.age;
    };
    "${host-id}-litellm-chatgpt-api-key-environment-variable" = {
      generator = {
        script = "environment-variable";
        dependencies = [
          config.age.secrets."${host-id}-litellm-chatgpt-api-key"
        ];
      };
      settings.field = "OPENAI_API_KEY";
      rekeyFile =
        ../secrets/${host-id}-litellm-chatgpt-api-key-environment-variable.age;
    };
    "${host-id}-litellm-master-key" = {
      generator.script = "hex";
      settings.length = 32;
      rekeyFile = ../secrets/${host-id}-litellm-master-key.age;
    };
    "${host-id}-litellm-master-key-environment-variable" = {
      generator = {
        script = "environment-variable";
        dependencies = [
          config.age.secrets."${host-id}-litellm-master-key"
        ];
      };
      settings.field = "LITELLM_MASTER_KEY";
      rekeyFile =
        ../secrets/${host-id}-litellm-master-key-environment-variable.age;
    };
    "${host-id}-litellm-salt-key" = {
      generator.script = "hex";
      settings.length = 32;
      rekeyFile = ../secrets/${host-id}-litellm-salt-key.age;
    };
    "${host-id}-litellm-salt-key-environment-variable" = {
      generator = {
        script = "environment-variable";
        dependencies = [
          config.age.secrets."${host-id}-litellm-salt-key"
        ];
      };
      settings.field = "LITELLM_SALT_KEY";
      rekeyFile =
        ../secrets/${host-id}-litellm-salt-key-environment-variable.age;
    };
  };
  services.https.fqdns."litellm.proton" = {
    enable = true;
    internalPort = config.services.litellm.port;
  };
  services.litellm = {
    enable = true;
    environment = {
      ANONYMIZED_TELEMETRY = "False";
      DO_NOT_TRACK = "True";
      DATABASE_URL = "postgres:///litellm?host=/run/postgresql";
      # TODO: You can run LiteLLM without this.  Contribute this back.
      # Otherwise we fail with trying to write to `/.cache`.
      HOME = "/var/lib/litellm";
      # TODO: You can run LiteLLM without this.  Contribute this back.
      # These are needed otherwise Prisma wants to install nodeenv so it can
      # later install Node.JS, and then (eventually) install its engines.
      PRISMA_QUERY_ENGINE_BINARY = "${pkgs.prisma-engines}/bin/query-engine";
      PRISMA_MIGRATION_ENGINE_BINARY = "${pkgs.prisma-engines}/bin/migration-engine";
      PRISMA_SCHEMA_ENGINE_BINARY = "${pkgs.prisma-engines}/bin/schema-engine";
      # TODO: You can run LiteLLM without this.  Contribute this back.
      # Otherwise we fail with trying to write to `/.cache`.
      PRISMA_CACHE_DIR = "/var/cache/prisma";
      SCARF_NO_ANALYTICS = "True";
      # TODO: You can run LiteLLM without this.  Contribute this back.
      # Otherwise we fail with trying to write to `/.cache`.
      XDG_CACHE_HOME = "/var/cache/litellm";
    };
    environmentFile = pkgs.writeTextFile {
      name = "litellm-environment-file.env";
      text = ''
        OPENAI_API_KEY="$(cat /run/credentials/litellm.service/OPENAI_API_KEY)"
        LITELLM_MASTER_KEY="$(cat /run/credentials/litellm.service/LITELLM_MASTER_KEY)"
        LITELLM_SALT_KEY="$(cat /run/credentials/litellm.service/LITELLM_SALT_KEY)"
      '';
    };
    settings = {
      # LiteLLM behavior hardening.
      litellm_settings = {
        drop_params = true;          # strip unknown params at the proxy
        set_verbose = false;         # quiet logs
        timeout = 60;                # seconds; request timeout
        num_retries = 2;             # minimal retrying
      };
      model_list = [
        {
          model_name = "gpt-5";
          litellm_params = {
            model = "gpt-5";
            api_key = "os.environ/OPENAI_API_KEY";
          };
        }
      ];
      # No routing/fallbacks needed if you’re only on OpenAI, but stub it out
      # for later.
      router_settings = { routing_strategy = "simple"; };
    };
  };
  systemd.services.litellm = {
    after = [
      "run-agenix.d.mount"
    ];
    requires = [
      "run-agenix.d.mount"
    ];
    # TODO: You can run LiteLLM without this.  Contribute this back.
    path = [
      pkgs.prisma
      pkgs.prisma-engines
    ];
    serviceConfig = {
      LoadCredential = [
        "OPENAI_API_KEY:${config.age.secrets."${host-id}-litellm-chatgpt-api-key".path}"
        "LITELLM_MASTER_KEY:${config.age.secrets."${host-id}-litellm-master-key".path}"
        "LITELLM_SALT_KEY:${config.age.secrets."${host-id}-litellm-salt-key".path}"
      ];
      # TODO: You can run LiteLLM without this.  Contribute this back.
      # Otherwise it tries to write to the `/.cache`.
      CacheDirectory = "litellm";
    };
  };
  services.postgresql = {
    enable = true;
    # package = pkgs.postgresql_16;
    ensureDatabases = [ "litellm" ];
    ensureUsers = [
      {
        name = "litellm";
        ensureDBOwnership = true;
      }
    ];
  };
}
