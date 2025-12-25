{ config, facts, host-id, lib, ... }: {
  imports = [
    ./wireguard-agenix-rekey-generator.nix
  ];
  age.secrets."${host-id}-wireguard-client" = {
    generator.script = "wireguard-priv";
    rekeyFile = ../secrets/${host-id}-wireguard-client.age;
  };
  # At least one host needs to make an accounting for the non-NixOS / nix-darwin
  # hosts that will join the VPN.  This should become a dynamic list, but that
  # will require some legerdemain for another day.
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
  networking.wg-quick = let
    vpn-host-ip = (lib.pipe facts.network.users [
      (lib.mapAttrsToList (name: u: u.devices))
      lib.lists.flatten
      (lib.lists.findFirst (d: d.host-id == host-id) null)
      # TODO: Do something like `or throwError "Could not find ${host-id} in
      # devices."` for better error handling.
    ]).ip;
  in {
    interfaces = {
      proton = {
        autostart = false;
        address = [
          "192.168.102.${vpn-host-ip}"
        ];
        dns = [ "192.168.254.1" ];
        listenPort = 51820;
        # null means automatic for MTU settings.
        mtu = null;
        peers = [
          {
            allowedIPs = [
              "192.168.254.0/24"
              "192.168.102.0/24"
            ];
            endpoint = "vpn.logustus.com:51820";
            persistentKeepalive = null;
            presharedKeyFile = null;
            publicKey = builtins.readFile ../secrets/silicon-wireguard-server.pub;
          }
        ];
        privateKeyFile = config.age.secrets."${host-id}-wireguard-client".path;
      };
      proton-only = {
        autostart = false;
        address = [
          "192.168.102.${vpn-host-ip}"
        ];
        dns = [ "192.168.254.1" ];
        listenPort = 51820;
        # null means automatic for MTU settings.
        mtu = null;
        peers = [
          {
            allowedIPs = [
              "192.168.254.0/24"
              "192.168.102.0/24"
              # To pass everything through.
              "0.0.0.0/0"
            ];
            endpoint = "vpn.logustus.com:51820";
            persistentKeepalive = null;
            presharedKeyFile = null;
            publicKey = builtins.readFile ../secrets/silicon-wireguard-server.pub;
          }
        ];
        privateKeyFile = config.age.secrets."${host-id}-wireguard-client".path;
      };
    };
  };
}
