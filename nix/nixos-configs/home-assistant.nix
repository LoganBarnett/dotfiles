################################################################################
# Home Assistant coordinates a variety of IoT (Internet of Things) devices.
################################################################################
{ config, host-id, ... }: {
  disabledModules = [ "services/home-automation/home-assistant.nix" ];
  age.secrets.home-assistant-secrets = {
    generator = {
      script = "yaml-secret-file";
      dependencies = [
        config.age.secrets.home-assistant-client-secret
      ];
    };
    rekeyFile = ../secrets/home-assistant-secrets.age;
  };
  imports = [
    ../nixos-modules/home-assistant.nix
    ./home-assistant-secret-file.nix
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
      oidc = {
        client_id = "home-assistant";
        client_secret = "!secret home-assistant-client-secret";
        callback_url = "https://home-assistant.proton/auth/external/callback";
        scopes = [
          "openid"
          "email"
          "profile"
        ];
      };
      zwave_js = {
        usb_path = config.services.zwave-js.serialPort;
      };
    };
  };
}
