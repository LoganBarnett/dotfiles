################################################################################
# DNS Smart Block - LLM-powered DNS classification and blocking.
#
# Provides gaming and video streaming classifiers that watch Blocky DNS logs,
# classify domains using Ollama LLM, and serve dynamic blocklists via HTTP API.
################################################################################
{ config, ... }:
{
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

    # Exclude internal .proton hostnames from LLM classification.  These are
    # private infrastructure names that are never gaming or streaming sites, and
    # excluding them prevents the classifier from wasting LLM calls on them
    # while still recording every resolution in the event log.
    excludeSuffixes = [ ".proton" ];

    # Ollama LLM server configuration.
    ollama = {
      url = "https://ollama.proton";
      model = "llama3.2:3b";
    };

    # Enable gaming and video streaming classifiers.
    classifiers = {
      gaming = {
        enable = true;
        preset = "gaming";
        minConfidence = 0.8;
        ttlDays = 90;
      };
      video-streaming = {
        enable = true;
        preset = "video-streaming";
        minConfidence = 0.8;
        ttlDays = 90;
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
      # Changed from default 8080 to 8083 to avoid conflict with goss-exporter.
      adminBindPort = 8083;
    };

    # Use built-in PostgreSQL and NATS.
    database.enable = true;
    nats.enable = true;

    # Declarative classification overrides.  These are reconciled into the
    # database on each service start and never expire.
    provisionedClassifications = [
      {
        domain = "conjuguemos.com";
        classificationType = "all";
        isMatchingSite = false;
        reasoning = "This is a Spanish edutainment site endorsed by the school district.";
      }
      {
        domain = "codeload.github.com";
        classificationType = "all";
        isMatchingSite = false;
        reasoning = "GitHub code download CDN — never a gaming or streaming site.";
      }
      {
        domain = "github.com";
        classificationType = "gaming";
        isMatchingSite = false;
        reasoning = "GitHub is a software development platform, not a gaming site.";
      }
      {
        domain = "registry.ollama.ai";
        classificationType = "all";
        isMatchingSite = false;
        reasoning = "Ollama model registry — AI infrastructure, not a gaming or streaming site.";
      }
    ];

    # Blocky DNS integration. Automatically map locally enabled classifiers to
    # Blocky blacklist groups.
    integrations.blocky = {
      enable = true;
      blocklistUrl = "http://localhost:3000";
      autoMapAllBlocklists = true;
    };
  };

  # Register a tank volume so the DNS Smart Block database is exported before
  # each Restic backup.  There is no application data directory to back up
  # (the service manages its own storage internally), so backupData is false.
  tankVolumes.volumes.dns-smart-block = {
    pgDatabase = config.services.dns-smart-block.database.name;
    backupData = false;
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
