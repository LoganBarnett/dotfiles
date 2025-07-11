################################################################################
# Configuration for an Aeotec Z-Stick 7.
################################################################################
{ config, pkgs, ... }: let
  keys = [
    "legacy"
    "unauthenticated"
    "authenticated"
    "access-control"
  ];
  device-path = "/dev/serial/by-id/usb-Silicon_Labs_CP2102N_USB_to_UART_Bridge_Controller_64d2edfddf7ced11ac6adbf3fdf7b791-if00-port0";
in {
  age.secrets = (builtins.listToAttrs
    (builtins.map (key: {
      name = "aeotec-z-stick-7-zwave-js-${key}-key";
      value = {
        generator.script = "hex";
        settings.length = 16;
        rekeyFile = ../secrets/aeotec-z-stick-7-zwave-js-${key}-key.age;
      };
    }) keys)
  ) // {
    aeotec-z-stick-7-zwave-js-security-file = {
      generator = {
        script = "template-file";
        dependencies = builtins.map
          (key: config.age.secrets."aeotec-z-stick-7-zwave-js-${key}-key" )
          keys
        ;
      };
      rekeyFile = ../secrets/aeotec-z-stick-7-zwave-js-security-file.age;
      settings.templateFile = pkgs.writeTextFile {
        name = "zwave-js-security.json";
        text = ''
          {
            "securityKeys": {
              "S0_Legacy": "%aeotec-z-stick-7-zwave-js-legacy-key%",
              "S2_Unauthenticated": "%aeotec-z-stick-7-zwave-js-unauthenticated-key%",
              "S2_Authenticated": "%aeotec-z-stick-7-zwave-js-authenticated-key%",
              "S2_AccessControl": "%aeotec-z-stick-7-zwave-js-access-control-key%"
            }
          }
      '';
      };
    };
  };
  environment.systemPackages = [
    pkgs.minicom
  ];
  systemd.services.zwave-js = {
    serviceConfig = {
      LoadCredential = [
        "zwave-js-secret:${
          config.age.secrets.aeotec-z-stick-7-zwave-js-security-file.path
        }"
      ];
    };
  };
  # systemd.services.zwave-js-ui = {
  #   serviceConfig = {
  #     LoadCredential = builtins.map
  #       (key:
  #         "${key}:${
  #           config.age.secrets."aeotec-z-stick-7-zwave-js-${key}-key".path
  #         }"
  #       ) keys
  #     ;
  #     Environment = let
  #       dir = "/run/credentials/%n";
  #     in [
  #       "KEY_S0_Legacy=${dir}/legacy"
  #       "KEY_S2_Unauthenticated=${dir}/unauthenticated"
  #       "KEY_S2_Authenticated=${dir}/authenticated"
  #       "KEY_S2_AccessControl=${dir}/access-control"
  #     ];
  #   };
  # };
  # services.zwave-js-ui = {
  #   enable = true;
  #   serialPort = device-path;
  #   settings = {
  #   };
  # };
  services.zwave-js = {
    enable = true;
    serialPort = device-path;
    secretsConfigFile = "/run/credentials/zwave-js.service/zwave-js-secret";
    settings = {
      # As of [2025-07-10] the only documentation for configuration is sitting
      # here: https://github.com/zwave-js/zwave-js-server/blob/master/example_config.js
      # Do not follow the readme for true configuration structure, but it might
      # give some hints.
      logConfig = {
        enable = true;
        # Actually make this print 99.999999% of the logs when running as a
        # service?  Cool.
        forceConsole = true;
        # level = "debug";
        # Should be debug.  6 is "silly".
        level = 5;
      };
      serial = {
        port = device-path;
        # baudRate = 115200;
        baudRate = 9600;
      };
    };
  };

}
