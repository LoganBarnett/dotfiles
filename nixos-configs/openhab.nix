################################################################################
# OpenHab handles controlling and automating small household devices (IoT).  It
# is a competitor to Home Assistant, and the largest pull for me is that OpenHab
# uses configuration files it wishes to keep documented, as opposed to Home
# Assistant's recent declaration of abandoning a file based configuration in
# favor of click-ops.
################################################################################
{
  config,
  facts,
  flake-inputs,
  system,
  ...
}:
{
  imports = [
    flake-inputs.openhab-flake.nixosModules.${system}.openhab
  ];
  nixpkgs.overlays = [ flake-inputs.openhab-flake.overlays.default ];
  services.openhab.enable = true;
  services.https.fqdns."openhab.${facts.network.domain}" = {
    enable = true;
    internalPort = config.services.openhab.ports.http;
  };
}
