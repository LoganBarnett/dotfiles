di@{ config, lib, lib-custom, pkgs, ... }: {
  imports = [
    ../nixos-modules/chronicle-proxy.nix

    {
      services.https.fqdns."chronicle-proxy.proton" = {
        enable = true;
        internalPort = config.services.chronicle-proxy.port;
      };
      services.chronicle-proxy = {
        enable = true;
      };
    }

    (lib-custom.environment-file-for-service di {
      service = "chronicle-proxy";
      secrets = {
        openai-api-key = {
          rekeyFile = ../secrets/chronicle-proxy-openai-api-key.age;
        };
      };
    })

  ];
}
