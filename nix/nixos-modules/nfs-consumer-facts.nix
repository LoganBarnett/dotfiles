################################################################################
# Build consumer NFS mounts from shared facts. Feeds services.nfs-mount using
# facts.nfsVolumes entries that match this host's host-id.
################################################################################
{ config, facts, host-id, lib, pkgs, ... }:
let
  inherit (lib) mkOption mkIf types mkEnableOption mapAttrs';
  cfg = config.nfsConsumerFacts;

  # Filter facts for this consumer.
  myVolumes = lib.filter (v: v."host-id" == host-id) facts.nfsVolumes;

  # Convenience builders.
  mkMountName = v: v.volume;  # e.g., "gitea"
  mkMountPoint = v: "${cfg.baseMountDir}/${v.volume}";
  mkShare = v: "/tank/data/${v.volume}";
  mkWgIP = v: "10.100.0.${toString v.peerNumber}/24";
  wgPeerIP = "10.100.0.1/32";  # provider fixed per your scheme
  wgPrivPath = v: "/run/agenix/${v."host-id"}-nfs-wireguard-key";
  wgPeerPubPath = cfg.provider.wgPeerPublicKeyFile;
in
{
  options.nfsConsumerFacts = {
    enable = mkEnableOption ''
      Enable consumer mounts derived from facts.nfsVolumes for this host.
    '';

    baseMountDir = mkOption {
      type = types.path;
      default = "/mnt";
      description = ''
        Base directory under which each volume will be mounted. For example,
        "/mnt" results in "/mnt/<volume>".
      '';
      example = "/srv/mnt";
    };

    provider = {
      remoteHost = mkOption {
        type = types.str;
        description = ''
          Provider's public FQDN used for the WireGuard endpoint.
        '';
        example = "silicon.proton";
      };
      vpnHost = mkOption {
        type = types.str;
        description = ''
          Provider's VPN hostname used for the NFS mount target after the
          tunnel is up.
        '';
        example = "silicon-nas.proton";
      };
      wgPort = mkOption {
        type = types.port;
        default = 51820;
        description = ''
          Provider WireGuard UDP port.
        '';
        example = 51820;
      };
      providerHostId = mkOption {
        type = types.str;
        description = ''
          Provider host-id; used to locate the provider WireGuard public key
          at "../secrets/<providerHostId>-nfs-wireguard-key.pub".
        '';
        example = "silicon";
      };
      wgPeerPublicKeyFile = mkOption {
        type = types.path;
        default =
          ../secrets/${config.nfsConsumerFacts.provider.providerHostId}-nfs-wireguard-key.pub;
        description = ''
          Path to the provider WireGuard public key file. Defaults to a path
          based on providerHostId.
        '';
        example = "../secrets/silicon-nfs-wireguard-key.pub";
      };
    };
  };

  config = mkIf cfg.enable {
    # Build one mount per matching fact row.
    services.nfs-mount.mounts =
      mapAttrs' (_: v:
        lib.nameValuePair (mkMountName v) {
          enable = true;
          remoteHost = cfg.provider.remoteHost;
          vpnHost = cfg.provider.vpnHost;
          share = mkShare v;
          mountPoint = mkMountPoint v;
          preconditionFile = "nfs-share-working";
          wgPrivateKeyFile = wgPrivPath v;
          wgIP = mkWgIP v;
          wgPeerPublicKeyFile = wgPeerPubPath;
          wgPeerIP = wgPeerIP;
          wgPort = cfg.provider.wgPort;
          # Interface name kept short: "wgnfs-<volume>".
          wgInterfaceName = "wgnfs-${mkMountName v}";
          bindfs = {
            source = "${mkMountPoint v}-raw";
            target = mkShare v;
            user = v.user;
            group = v.group;
            createForUser = true;
            createForGroup = true;
          };
          bindToService = v.service;
        }
      )
      (lib.listToAttrs
        (map (v: lib.nameValuePair (mkMountName v) v) myVolumes));
  };
}
