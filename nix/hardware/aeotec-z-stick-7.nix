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
    aeotec-z-stick-7-zwave-js-ui-security-file = {
      generator = {
        script = "template-file";
        dependencies = builtins.map
          (key: config.age.secrets."aeotec-z-stick-7-zwave-js-${key}-key" )
          keys
        ;
      };
      rekeyFile = ../secrets/aeotec-z-stick-7-zwave-js-ui-security-file.age;
      settings.templateFile = pkgs.writeTextFile {
        name = "zwave-js-ui-security.json";
        text = ''
          {
            "zwave": {
              "securityKeys": {
                "S0_Legacy": "%aeotec-z-stick-7-zwave-js-legacy-key%",
                "S2_Unauthenticated": "%aeotec-z-stick-7-zwave-js-unauthenticated-key%",
                "S2_Authenticated": "%aeotec-z-stick-7-zwave-js-authenticated-key%",
                "S2_AccessControl": "%aeotec-z-stick-7-zwave-js-access-control-key%"
              },
              "securityKeysLongRange": {
                "S2_Authenticated": "%aeotec-z-stick-7-zwave-js-authenticated-key%",
                "S2_AccessControl": "%aeotec-z-stick-7-zwave-js-access-control-key%"
              }
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
    wants = [
      "run-agenix.d.mount"
    ];
    after = [
      "run-agenix.d.mount"
    ];
  };
  systemd.services.zwave-js-ui = {
    serviceConfig = {
      # LoadCredential = builtins.map
      #   (key:
      #     "${key}:${
      #       config.age.secrets."aeotec-z-stick-7-zwave-js-${key}-key".path
      #     }"
      #   ) keys
      # ;
      LoadCredential = [
        "zwave-js-secret:${
          config.age.secrets.aeotec-z-stick-7-zwave-js-ui-security-file.path
        }"
      ];
    };
    wants = [
      "run-agenix.d.mount"
    ];
    after = [
      "run-agenix.d.mount"
    ];
  };
  services.zwave-js-ui.serialPort = device-path;
  services.zwave-js-ui.settings2.zwave.port = device-path;
  services.zwave-js = {
    serialPort = device-path;
    settings.serial.port = device-path;
  };

}
