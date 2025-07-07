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
      };
      systemd.services.zwave-js = {
        serviceConfig = {
          LoadCredential = [
            "zwave-js-secret:${config.age.secrets.zwave-js-secret.path}"
          ];
        };
      };
      # We're short on total configuration here.  At the moment I get:
      # Error: `securityKeys.S0_Legacy` key is missing.
      # Which makes me think the configuration here is incomplete.
      services.zwave-js = {
        enable = true;
        serialPort = "/dev/serial/by-id/usb-Silicon_Labs_CP2102N_USB_to_UART_Bridge_Controller_64d2edfddf7ced11ac6adbf3fdf7b791-if00-port0";
        secretsConfigFile = "/run/credentials/zwave-js.service/zwave-js-secret";
      };
    })
    (import ../nixos-modules/https.nix {
      server-port = 3000;
      inherit host-id;
      fqdn = "${host-id}.proton";
    })
  ];
}
