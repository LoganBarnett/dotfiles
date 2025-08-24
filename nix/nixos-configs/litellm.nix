{ config, host-id, ... }: {
  age.secrets = {
    "${host-id}-litellm-chatgpt-api-key" = {
      rekeyFile = ../secrets/${host-id}-litellm-chatgpt-api-key.age;
    };
    "${host-id}-litellm-chatgpt-api-key-environment-variable" = {
      script = {
        generator = "environment-file";
        dependencies = [
          config.age.secrets."${host-id}-litellm-chatgpt-api-key"
        ];
      };
      field = "OPENAI_API_KEY";
      rekeyFile =
        ../secrets/${host-id}-litellm-chatgpt-api-key-environment-variable.age;
    };
    "${host-id}-litellm-master-key" = {
      script.generator = "hex";
      settings.length = 32;
      rekeyFile = ../secrets/${host-id}-litellm-master-key.age;
    };
    "${host-id}-litellm-master-key" = {
      script = {
        generator = "environment-file";
      };
      rekeyFile =
        ../secrets/${host-id}-litellm-master-key-environment-variable.age;
    };
    "${host-id}-litellm-master-key-environment-variable" = {
      script = {
        generator = "environment-file";
        dependencies = [
          config.age.secrets."${host-id}-litellm-master-key"
        ];
      };
      settings.field = "LITELLM_MASTER_KEY";
      rekeyFile =
        ../secrets/${host-id}-litellm-master-key-environment-variable.age;
    };
    "${host-id}-litellm-environment-file" = {
      script = {
        generator = "environment-file-aggregate";
        dependencies = [
          config.age.secrets."${host-id}-litellm-chatgpt-api-key-environment-variable"
          config.age.secrets."${host-id}-litellm-master-key-environment-variable"
        ];
      };
      rekeyFile =
        ../secrets/${host-id}-litellm-chatgpt-api-key-environment-variable.age;
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
      SCARF_NO_ANALYTICS = "True";
    };
    environmentFile = config
      .age
      .secrets
      ."${host-id}-litellm-environment-file"
      .path
    ;
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
          model_name = "gpt-5o";
          litellm_params = {
            model = "gpt-5o";
            api_key = "os.environ/OPENAPI_API_KEY";
          };
        }
      ];
      # No routing/fallbacks needed if you’re only on OpenAI, but stub it out
      # for later.
      router_settings = { routing_strategy = "simple"; };
    };
  };
}
