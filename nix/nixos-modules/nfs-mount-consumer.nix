################################################################################
# Provide a generalized means of consuming an NFS share. Authentication,
# authorization, and encryption are handled via WireGuard.
################################################################################
{ config, lib, pkgs, ... }: let
  inherit (lib) mkEnableOption mkIf mkOption mkMerge types literalExpression;
  cfg = config.services.nfs-mount;
  nfsMountModule = { name, config, ... }: let
    mountNameSanitized = builtins.replaceStrings
      ["-" "." "/"]
      ["_" "_" "_"]
      name
    ;
    mountUnitName = builtins.replaceStrings
      ["/" "-"]
      ["-" "\\x2d"]
      config.mountPoint
    ;
    defaultInterface = "wgnfs-${name}";
    wgInterface = config.wgInterfaceName;
  in {
    options = {
      enable = mkEnableOption ''
        Enable this NFS mount.
      '';

      remoteHost = mkOption {
        type = types.str;
        example = "silicon.proton";
        description = ''
          The remote NFS hostname, used in the WireGuard endpoint.
        '';
      };

      vpnHost = mkOption {
        type = types.str;
        example = "silicon-nas.proton";
        description = ''
          The hostname used to reach the NFS share once the VPN is up.
        '';
      };

      share = mkOption {
        type = types.str;
        example = "/tank/data/foo";
        description = ''
          The exported NFS share path on the remote host.
        '';
      };

      mountPoint = mkOption {
        type = types.path;
        example = "/mnt/foo";
        description = ''
          The local mount point for this NFS share.
        '';
      };

      wgPrivateKeyFile = mkOption {
        type = types.path;
        example = "/run/agenix/nextcloud-nfs-wireguard-key";
        description = ''
          Path to the WireGuard private key (use with agenix).
        '';
      };

      wgIP = mkOption {
        type = types.str;
        example = "10.100.0.3/24";
        description = ''
          IP address for the local WireGuard interface.
        '';
      };

      wgPeerPublicKeyFile = mkOption {
        type = types.path;
        example = "../secrets/silicon-nfs-wireguard-key.pub";
        description = ''
          Path to the peer's public key file.
        '';
      };

      wgPeerIP = mkOption {
        type = types.str;
        example = "10.100.0.1/32";
        description = ''
          IP address for the remote WireGuard peer.
        '';
      };

      wgPort = mkOption {
        type = types.port;
        default = 51820;
        description = ''
          The WireGuard peer endpoint port.
        '';
      };

      wgInterfaceName = mkOption {
        type = types.str;
        default = defaultInterface;
        example = "wgnfs-myshare";
        description = ''
          The name of the WireGuard interface to create. Must be 15 characters
          or fewer to be accepted by the Linux kernel.
        '';
      };

      bindfs = mkOption {
        type = types.nullOr (types.attrsOf types.str);
        default = null;
        example = {
          group = "nextcloud";
          user = "nextcloud";
        };
        description = ''
          Optional bindfs remapping. Specify uid/gid remaps and optionally
          extra options.
        '';
      };

      preconditionFile = mkOption {
        type = types.str;
        default = "nfs-share-working";
        description = ''
          Filename to check as a mount sanity test.
        '';
      };

      bindToService = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "gitea";
        description = ''
          Optional systemd service to bind this mount to. This ensures the
          mount is available before the service starts. Also injects
          BindPaths=''${mountPoint} into the serviceConfig.
        '';
      };
    };

    config = mkIf config.enable {
      assertions = [
        {
          assertion = builtins.stringLength wgInterface <= 15;
          message = ''
            WireGuard interface name '${wgInterface}' exceeds 15 characters.
          '';
        }
      ];

      fileSystems."${config.mountPoint}-raw" = {
        device = "${config.vpnHost}:${config.share}";
        fsType = "nfs";
        options = [
          "defaults"
          "noatime"
          "_netdev"
          "x-systemd.requires=network-online.target"
          "x-systemd.after=network-online.target"
        ];
        neededForBoot = false;
      };

      networking.hosts."${builtins.match ".*?([0-9.]+).*" config.wgPeerIP}" = [
        config.vpnHost
      ];

      networking.wireguard.enable = true;
      networking.wireguard.interfaces.${wgInterface} = {
        ips = [ config.wgIP ];
        privateKeyFile = config.wgPrivateKeyFile;
        peers = [{
          name = "nfs-${name}";
          publicKey = builtins.readFile config.wgPeerPublicKeyFile;
          endpoint = "${config.remoteHost}:${toString config.wgPort}";
          allowedIPs = [ config.wgPeerIP ];
          persistentKeepalive = 25;
        }];
      };

      systemd.mounts = [{
        name = "${mountUnitName}-raw.mount";
        what = "${config.vpnHost}:${config.share}";
        where = "${config.mountPoint}-raw";
        type = "nfs";
        options = lib.strings.concatStringsSep "," [
          "defaults"
          "noatime"
          "_netdev"
          "x-systemd.requires=network-online.target"
          "x-systemd.after=network-online.target"
        ];
      }];

      systemd.automounts = [{
        name = "${mountUnitName}.automount";
        where = config.mountPoint;
        after = [ "network-online.target" ];
        requires = [ "network-online.target" ];
        automountConfig.TimeoutIdleSec = "600";
      }];

      bindfs-mappings.${name} = cfg.bindfs;
      systemd.services = lib.mkIf (config.bindToService != null) {
        "${config.bindToService}".after = [
          "mnt-${mountUnitName}-raw.mount"
          "nfs-mount-${name}-bindfs.service"
          "nfs-mount-${name}-sanity-check.service"
        ];
        "${config.bindToService}".requires = [
          "mnt-${mountUnitName}-raw.mount"
          "nfs-mount-${name}-bindfs.service"
          "nfs-mount-${name}-sanity-check.service"
        ];
        "${config.bindToService}".serviceConfig.BindPaths = [
          config.mountPoint
        ];
        "nfs-mount-${name}-sanity-check" = mkIf
          (config.preconditionFile != null)
          {
            after = [ "nfs-mount-${name}-bindfs.service" ];
            wantedBy = [ "multi-user.target" ]
                       ++ lib.optional (config.bindToService != null)
                         "${config.bindToService}.service";
            serviceConfig = {
              Type = "oneshot";
            };
            script = ''
            stat "${config.mountPoint}/${config.preconditionFile}"
          '';
          };
        "nfs-mount-${name}-bindfs" = mkIf (config.bindfs != null) {
          after = [ "${mountUnitName}-raw.mount" ];
          wantedBy = [ "multi-user.target" ]
                     ++ lib.optional (config.bindToService != null)
                       "${config.bindToService}.service";
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          script = ''
          ${pkgs.bindfs}/bin/bindfs \
            --force-user=${config.bindfs.uid or "nobody"} \
            --force-group=${config.bindfs.gid or "nogroup"} \
            ${lib.optionalString (config.bindfs.extraOptions or null != null)
              (builtins.concatStringsSep " " config.bindfs.extraOptions)} \
            "${config.mountPoint}-raw" "${config.mountPoint}"
        '';
        };
      };
    };
  };

in {
  options.services.nfs-mount.mounts = mkOption {
    type = types.attrsOf (types.submodule nfsMountModule);
    default = {};
    description = ''
      A set of NFS mount definitions handled over WireGuard.
    '';
  };

  # config = mkMerge (lib.attrValues cfg.mounts);
}
