di@{ config, lib, lib-custom, pkgs, ... }: {
  imports = [
    ../nixos-modules/chronicle-proxy.nix
    ../nixos-modules/environment-file-secrets.nix
  ];
  age.secrets = {
    openai-api-key = {
      rekeyFile = ../secrets/chronicle-proxy-openai-api-key.age;
    };
  };
  services.https.fqdns."chronicle-proxy.proton" = {
    enable = true;
    internalPort = config.services.chronicle-proxy.port;
  };
  services.chronicle-proxy = {
    enable = true;
  };
  services.environment-file-secrets.services.chronicle-proxy.secrets = {
    openai-api-key = {};
  };
  # Goss health checks for Chronicle Proxy.
  services.goss.checks = {
    # Check that the HTTPS endpoint is responding.  We use the root endpoint
    # as a basic health check since Chronicle Proxy doesn't implement all
    # OpenAI API endpoints.
    http."https://chronicle-proxy.proton/" = {
      status = 200;
      timeout = 5000;
    };
    # Check that the internal chronicle-proxy port is listening.
    port."tcp:${toString config.services.chronicle-proxy.port}" = {
      listening = true;
      ip = [ config.services.chronicle-proxy.host ];
    };
    # Check that HTTPS port is listening (handled by reverse proxy).
    port."tcp:443" = {
      listening = true;
      ip = [ "0.0.0.0" ];
    };
    # Check that the chronicle-proxy service is running.
    service.chronicle-proxy = {
      enabled = true;
      running = true;
    };
  };
}
