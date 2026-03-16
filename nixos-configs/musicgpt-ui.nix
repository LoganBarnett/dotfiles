{ config, ... }: {
  imports = [
    ../nixos-modules/musicgpt-ui.nix
  ];
  services.https.fqdns."musicgpt.proton" = {
    enable = true;
    internalPort = config.services.musicgpt-ui.port;
  };
  services.musicgpt-ui = {
    enable = true;
    model = "large";
  };
}
