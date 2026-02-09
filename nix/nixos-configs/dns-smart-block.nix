################################################################################
# DNS Smart Block - LLM-powered DNS classification and blocking.
#
# Provides gaming and video streaming classifiers that watch Blocky DNS logs,
# classify domains using Ollama LLM, and serve dynamic blocklists via HTTP API.
################################################################################
{ config, ... }: {
  # Admin interface secrets for HTTP Basic Auth.
  age.secrets = {
    dns-smart-block-admin-password = {
      generator.script = "long-passphrase";
    };
    dns-smart-block-admin-htpasswd = {
      generator = {
        script = "htpasswd";
        dependencies = [
          config.age.secrets.dns-smart-block-admin-password
        ];
      };
      settings = {
        username = "admin";
      };
      mode = "0440";
      owner = "nginx";
      group = "nginx";
    };
  };

  # DNS Smart Block - LLM-powered DNS blocking with gaming and video streaming
  # classification.
  services.dns-smart-block = {
    enable = true;

    # Ollama LLM server configuration.
    ollama = {
      url = "http://M-CL64PK702X.proton:11434";
      model = "llama3.2:3b";
    };

    # Enable gaming and video streaming classifiers.
    classifiers = {
      gaming = {
        enable = true;
        preset = "gaming";
        minConfidence = 0.8;
        ttlDays = 10;
      };
      video-streaming = {
        enable = true;
        preset = "video-streaming";
        minConfidence = 0.8;
        ttlDays = 10;
      };
    };

    # Watch Blocky DNS logs for domains to classify.
    logProcessor = {
      enable = true;
      logSource = "cmd:journalctl -f -u blocky";
    };

    # Serve blocklists via HTTP API. Bind to all interfaces to allow
    # Prometheus scraping from other hosts.
    blocklistServer = {
      enable = true;
      publicBindHost = "0.0.0.0";
      publicBindPort = 3000;
    };

    # Use built-in PostgreSQL and NATS.
    database.enable = true;
    nats.enable = true;

    # Blocky DNS integration. Automatically map locally enabled classifiers to
    # Blocky blacklist groups.
    integrations.blocky = {
      enable = true;
      blocklistUrl = "http://localhost:3000";
      autoMapAllBlocklists = true;
    };
  };

  # TLS termination for admin interface.
  services.https.fqdns."dns-smart-block.proton" = {
    enable = true;
    internalPort = config.services.dns-smart-block.blocklistServer.adminBindPort;
  };

  # Nginx proxy configuration for admin interface with HTTP Basic Auth.
  services.nginx.virtualHosts."dns-smart-block.proton" = {
    locations."/" = {
      # proxyPass = "http://${config.services.dns-smart-block.blocklistServer.adminBindHost}:${toString config.services.dns-smart-block.blocklistServer.adminBindPort}";
      basicAuth = "DNS Smart Block Admin";
      basicAuthFile = config.age.secrets.dns-smart-block-admin-htpasswd.path;
    };
  };
}
