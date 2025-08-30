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
}
