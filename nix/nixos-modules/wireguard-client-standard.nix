{ config, host-id, ... }: {
  age.secrets."${host-id}-wireguard-client" = {
    generator.script = "wireguard-priv";
    rekeyFile = ../secrets/${host-id}-wireguard-client.age;
  };
  age.secrets."manganese-wireguard-client" = {
    generator.script = "wireguard-priv";
    rekeyFile = ../secrets/manganese-wireguard-client.age;
  };
  nixpkgs.overlays = [
    (final: prev: {
      wireguard-tools = prev.wireguard-tools.overrideAttrs (old: {
        patches = (old.patches or []) ++ [
          # Mana from the heavens.  Show us what file Wireguard is complaining
          # about.  Found here:
          # https://lists.zx2c4.com/pipermail/wireguard/2024-April/008535.html
          ./wg-file-path-in-errors.patch
        ];
      });
    })
  ];

  # I got this from looking at
  # https://github.com/LnL7/nix-darwin/blob/ae406c04577ff9a64087018c79b4fdc02468c87c/modules/services/wg-quick.nix#L43
  # which is for nix-darwin.  I should look to see if this interface matches
  # that of the NixOS module, assuming it even exists.
  networking.wg-quick = {
    interfaces = {
      proton-only = {
        autostart = false;
        address = [
          # Fix!!!  This is for manganese.
          "192.168.102.22"
        ];
        dns = [ "192.168.254.254" ];
        listenPort = 51820;
        # null means automatic for MTU settings.
        mtu = null;
        # I think there's a bug.  PostUp is what is used to setup the private
        # key, but how can that be the case when the interface would have to
        # already be up using said private key?
        # preUp = ''
        #   wg set \
        #    proton-only \
        #    private-key \
        #    ${config.networking.wg-quick.interfaces.proton-only.privateKeyFile}
        # '';
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
        # Work around the fact that iOS doesn't feed tethering data through the
        # VPN routing until I can get a direct setup for this host working.
        privateKeyFile = config.age.secrets."manganese-wireguard-client".path;
        # privateKeyFile = config.age.secrets."${host-id}-wireguard-client".path;
      };
    };
  };
}
