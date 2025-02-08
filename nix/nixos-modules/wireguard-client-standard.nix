{ config, host-id, ... }: {
  # I got this from looking at
  # https://github.com/LnL7/nix-darwin/blob/ae406c04577ff9a64087018c79b4fdc02468c87c/modules/services/wg-quick.nix#L43
  # which is for nix-darwin.  I should look to see if this interface matches
  # that of the NixOS module, assuming it even exists.
  networking.wg-quick = {
    interfaces = {
      proton-only = {
        autostart = false;
        dns = [ "192.168.254.254" ];
        listenPort = 51820;
        # null means automatic for MTU settings.
        mtu = null;
        peers = [
          {
            allowedIPs = [
              "192.168.254.0/24"
              "192.168.102.0/24"
              # To pass everything through.
              # "0.0.0.0/0"
            ];
            endpoint = "50.39.134.127:51820";
            persistentKeepalive = null;
            presharedKeyFile = null;
            publicKey = builtins.readFile ../secrets/argon-wireguard-server.pub;
          }
        ];

        privateKeyFile = config.secrets."${host-id}-wireguard-client".path;
      };
    };
  };
}
