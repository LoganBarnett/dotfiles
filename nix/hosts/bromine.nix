################################################################################
# Bromine is a trace element that is highly reactive and thus isn't found freely
# in nature.  It was used long ago as a sedative and can help people with
# epilepsy.
################################################################################
{ flake-inputs, host-id, system, ... }: {
  imports = [
    ../nixos-modules/raspberry-pi-4.nix
    # ../nixos-modules/nix-builder-provide.nix
    # This seems to fail with the generated key.
    # ../nixos-modules/raspberry-pi-builder.nix
    ../nixos-modules/server-host.nix
    ../nixos-configs/matrix-server.nix
    ../nixos-configs/home-assistant.nix
    {
      networking.hostId = "37fab2ca";
      nixpkgs.hostPlatform = system;
    }
    ({ config, ... }: {
      age.secrets.zwave-js-secret = {
        rekeyFile = ../secrets/zwave-js-secret.age;
        group = toString config.services.zwave-js.port;
        mode = "0440";
      };
      services.zwave-js = {
        enable = true;
        serialPort = "/dev/serial/by-id/usb-Silicon_Labs_CP2102N_USB_to_UART_Bridge_Controller_64d2edfddf7ced11ac6adbf3fdf7b791-if00-port0";
        secretsConfigFile = config.age.secrets.zwave-js-secret.path;
      };
    })
    (import ../nixos-modules/https.nix {
      server-port = 3000;
      inherit host-id;
      fqdn = "${host-id}.proton";
    })
  ];
}
