################################################################################
# Bromine is a trace element that is highly reactive and thus isn't found freely
# in nature.  It was used long ago as a sedative and can help people with
# epilepsy.
################################################################################
{
  flake-inputs,
  host-id,
  system,
  ...
}:
{
  imports = [
    ../nixos-configs/grafana-kiosk-overview.nix
    ../nixos-configs/raspberry-pi-usb-disk.nix
    ../nixos-configs/raspberry-pi-4.nix
    ../nixos-modules/linux-host.nix
    # ../nixos-configs/home-assistant.nix
    # ../nixos-configs/mosquitto.nix
    # ../nixos-configs/openhab.nix
    # ../nixos-configs/zwave-js-server.nix
    # ../nixos-configs/zwave-js-ui.nix
    # ../nixos-configs/matrix-signal-bridge.nix
    {
      networking.hostId = "37fab2ca";
      nixpkgs.hostPlatform = system;
    }
  ];
}
