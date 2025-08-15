################################################################################
# Bromine is a trace element that is highly reactive and thus isn't found freely
# in nature.  It was used long ago as a sedative and can help people with
# epilepsy.
################################################################################
{ flake-inputs, host-id, system, ... }: {
  imports = [
    ../nixos-modules/raspberry-pi-4.nix
    ../nixos-modules/server-host.nix
    ../nixos-configs/matrix-server.nix
    ../nixos-configs/home-assistant.nix
    ../nixos-configs/mosquitto.nix
    # ../nixos-configs/zwave-js-server.nix
    ../nixos-configs/zwave-js-ui.nix
    ../hardware/aeotec-z-stick-7.nix
    # ../nixos-configs/matrix-signal-bridge.nix
    {
      networking.hostId = "37fab2ca";
      nixpkgs.hostPlatform = system;
    }
    (import ../nixos-modules/https.nix {
      server-port = 3000;
      inherit host-id;
      fqdn = "${host-id}.proton";
    })
  ];
}
