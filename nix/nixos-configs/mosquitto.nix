################################################################################
# Mosquitto is an MQTT broker.  MQTT itself a pub/sub protocol not unlike
# ActiveMQ, but with a lightweight focus which makes it ideal for IoT devices.
#
# Through Mosquitto, we can manage credentials to various devices.
################################################################################
{ config, lib, pkgs, ... }: {
  tls.tls-leafs."mosquitto.proton" = {
    fqdn = "mosquitto.proton";
    ca = config.age.secrets.proton-ca;
  };
  # systemd.services.mosquitto.serviceConfig = {
  #   LoadCredential = [
  #     "mosquitto-password-file:${
  #       config.age.secrets.mosquitto-password-file.path
  #     }"
  #   ];
  # };
  imports = [
    ./mosquitto-secrets.nix
  ];
  services.mosquitto = let
    password-file =
      "/run/credentials/mosquitto.service/mosquitto-password-file";
  in {
    enable = true;
    listeners = [
      {
        settings = {
          allow_anonymous = false;
        };
        users = {
          home-assistant = {
            passwordFile =
              config.age.secrets.mosquitto-home-assistant-password.path;
          };
          tasmota-master-bedroom-led-strip = {
            passwordFile = config
              .age
              .secrets
              .mosquitto-tasmota-master-bedroom-led-strip-password
              .path;
          };
        };
      }
    ];
    # For these purposes, I don't see the need to write to disk.  If you missed
    # the message, it's too late.
    persistence = false;
  };
}
