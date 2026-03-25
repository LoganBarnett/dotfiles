{ config, facts, ... }:
{
  imports = [
    ../nixos-modules/musicgpt-ui.nix
  ];
  services.https.fqdns."musicgpt.${facts.network.domain}" = {
    enable = true;
    internalPort = config.services.musicgpt-ui.port;
  };
  services.musicgpt-ui = {
    enable = true;
    model = "large";
  };
}
