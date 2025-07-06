################################################################################
# Home Assistant coordinates a variety of IoT (Internet of Things) devices.
################################################################################
{ config, host-id, ... }: {
  imports = [
    (import ../nixos-modules/https.nix {
      server-port = config.services.home-assistant.config.http.server_port;
      inherit host-id;
      fqdn = "home-assistant.proton";
      # redirect = false;
    })
  ];
  services.home-assistant = {
    enable = true;
    extraComponents = [ "frontend" "http" ];
    config = {
      default_config = {};
      http = {
        trusted_proxies = [
          "127.0.0.1"
          "::1"
        ];
        use_x_forwarded_for = true;
      };
      zwave_js = {
        usb_path = config.services.zwave-js.serialPort;
      };
    };
  };
}
