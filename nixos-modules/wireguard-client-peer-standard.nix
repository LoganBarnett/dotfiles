################################################################################
# Declare a Wireguard peer as a client that will connect to our server.  Uses
# the "standard" version instead of network.
#
# This is intended to be consumed directly via wireguard-server-standard.nix.
################################################################################
({ host-id, ip, vpn-subnet-prefix }: { config, lib, ... }: {
  imports = [
    ./wireguard-agenix-rekey-generator.nix
  ];
  age.secrets."${host-id}-wireguard-client" = {
    generator.script = "wireguard-priv";
    rekeyFile = ../secrets/${host-id}-wireguard-client.age;
  };
  systemd.network.netdevs."50-wg0" = {
    # Options are not entirely evaluated after all configurations have been
    # merged.  This module needs to configure netdevConfig or the option
    # processing will fail.  To work around this, we provide a dummy value
    # (which is really close to the real value for belt-and-suspender purposes
    # only).  The override is set to be lower priority than the default option
    # priority, and the server module sets the real value we actually want to
    # use.
    netdevConfig = lib.mkOverride 1501 { Kind = "wireguard"; Name = "wg0"; };
    wireguardPeers = [
      {
        PublicKey = config.age.secrets."${host-id}-wireguard-client".path;
        AllowedIPs = [
          "${vpn-subnet-prefix}.${ip}"
        ];
      }
    ];
  };
  # Old mode and might need to be disabled.
  # networking.wireguard.interfaces.wg0 = {
  #   peers = [
  #     {
  #       publicKey = config.age.secrets."${host-id}-wireguard-client".path;
  #       allowedIPs = [
  #         "${vpn-subnet-prefix}.${ip}"
  #       ];
  #     }
  #   ];
  # };
})
