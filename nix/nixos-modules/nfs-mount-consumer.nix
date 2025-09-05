################################################################################
# Provide a generalized means of consuming an NFS share. Authentication,
# authorization, and encryption are handled via WireGuard.
#
# We create several mounts here for an individual binding.  The "raw" mount is
# done to establish an NFS mount via `fileSystems` and `automounts`.  A
# `bindfs-mappings` mount is also created so we can expose permissions properly
# to the service in question.
################################################################################
{ config, host-id, lib, pkgs, ... }: let
  inherit (lib) mkEnableOption mkIf mkOption mkMerge types literalExpression;
  inherit (lib.attrsets) mapAttrsToList mapAttrs mapAttrs' foldlAttrs;
  inherit (lib) optional;
  # Derive a systemd .mount unit name from a path, like `systemd-escape --path
  # --suffix=mount`.
  toMountUnit =
    path:
    let
      p = toString path;
      normalize =
        s:
        # Strip a single trailing slash (except root) and the leading slash.
        let
          noTrail = if s != "/" && lib.hasSuffix "/" s
                    then lib.removeSuffix "/"
                    else s;
          noLead  = if lib.hasPrefix "/" noTrail
                    then lib.removePrefix "/" noTrail
                    else noTrail;
        in
          noLead;
      stem0 = builtins.replaceStrings [ "/" ] [ "-" ] (normalize p);
      # Escape literal '-' as \x2d to avoid ambiguity with '/'.
      stem  = builtins.replaceStrings [ "-" ] [ "\\x2d" ] stem0;
    in
      if p == "/"
      then "-.mount"
      else "${stem}.mount";
  cfg = config.services.nfs-mount;
  mountsEnabled = lib.filterAttrs (_: m: m.enable or false) cfg.mounts;
  rawWhereOf = m: "${toString m.mountPoint}-raw";
  rawUnitOf  = m: "${toMountUnit (rawWhereOf m)}";
  peerIPv4Of = m: builtins.elemAt
    (builtins.match "([^/]*)/.*" m.wgPeerIP)
    0
  ;
  nfsMountModule = { name, _config, ... }: let
    defaultInterface = "wgnfs-${name}";
  in {
    options = {
        enable = (mkEnableOption ''
          Enable this NFS mount.
        '') // { default = true; };

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

    # bindfs = mkOption {
    #   type = types.nullOr (types.attrsOf types.str);
    #   default = null;
    #   example = {
    #     group = "nextcloud";
    #     user = "nextcloud";
    #   };
    #   description = ''
    #     Optional bindfs remapping. Specify uid/gid remaps and optionally
    #     extra options.
    #   '';
    # };

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
  };

  assertions =
    mapAttrsToList (_: m: {
      assertion = builtins.stringLength m.wgInterfaceName <= 15;
      message = ''
        WireGuard interface name '${m.wgInterfaceName}' exceeds 15 characters.
      '';
    }) mountsEnabled
    ++ (let
         ifaces = mapAttrsToList (_: m: m.wgInterfaceName) mountsEnabled;
       in lib.optional (lib.length ifaces != lib.length (lib.unique ifaces)) {
            assertion = false;
            message = ''
              Duplicate wgInterfaceName across services.nfs-mount.mounts.
            '';
          });

  # ----- networking.hosts (merge by IP; collect vpnHost list)
  networkingHosts =
    foldlAttrs (acc: _: m:
      let ip = peerIPv4Of m;
      in acc // { ${ip} = lib.unique ((acc.${ip} or []) ++ [ m.vpnHost ]); }
    ) {} mountsEnabled;

  # ----- wireguard.interfaces (keyed by wgInterfaceName)
  wgInterfaces =
    foldlAttrs (acc: name: m:
      acc // {
        ${m.wgInterfaceName} = {
          ips            = [ m.wgIP ];
          privateKeyFile = m.wgPrivateKeyFile;  # secret path, not read
          peers = [{
            name       = "nfs-${name}";
            publicKey  = builtins.readFile m.wgPeerPublicKeyFile; # public only
            endpoint   = "${m.remoteHost}:${toString m.wgPort}";
            allowedIPs = [ m.wgPeerIP ];
            persistentKeepalive = 25;
          }];
        };
      }
    ) {} mountsEnabled;

  # ----- systemd.mounts / automounts (lists)
  mountsList =
    mapAttrsToList (_: m: {
      where   = rawWhereOf m;
      what    = "${m.vpnHost}:${m.share}";
      type    = "nfs";
      options = lib.concatStringsSep "," [
        "defaults" "noatime" "_netdev"
        "x-systemd.requires=network-online.target"
        "x-systemd.after=network-online.target"
      ];
    }) mountsEnabled;

  automountsList =
    mapAttrsToList (_: m: {
      where = m.mountPoint;
      after = [ "network-online.target" ];
      requires = [ "network-online.target" ];
      automountConfig.TimeoutIdleSec = "600";
    }) mountsEnabled;

  # ----- bindfs-mappings (attrset) for your other module
  bindfsMappings =
    mapAttrs
      (name: m: {
        source = rawWhereOf m;
        target = m.mountPoint;
        createForGroup = true;
        group = m.bindToService;
      })
      mountsEnabled
  ;

  # ----- service merges (handle multi-mount → same service)
  mergeSvc = svcOld: svcAdd:
    let
      afterMerged    = lib.unique ((svcOld.after or []) ++ (svcAdd.after or []));
      requiresMerged = lib.unique ((svcOld.requires or []) ++ (svcAdd.requires or []));
      scOld = svcOld.serviceConfig or {};
      scAdd = svcAdd.serviceConfig or {};
      # For lists inside serviceConfig we keep ‘last wins’ unless you want custom concat.
    in svcOld // svcAdd // {
      after = afterMerged;
      requires = requiresMerged;
      serviceConfig = scOld // scAdd;
    };

  servicesFromBinds =
    foldlAttrs (acc: name: m:
      if m.bindToService == null then acc else
      let
        svc = m.bindToService;
        entry = let
          after = [
            (rawUnitOf m)
            "nfs-mount-${name}-bindfs.service"
            "nfs-mount-${name}-sanity-check.service"
          ];
        in {
          inherit after;
          requires = after;
          serviceConfig = {
            DynamicUser = true;
            ProtectSystem = "strict";
            ReadWritePaths = [ m.mountPoint ];
            PrivateTmp = true;
            NoNewPrivileges = true;
            # Just assume the service name is an appropriate group name.
            SupplementaryGroups = [ m.bindToService ];
          };
        };
        prev = acc.${svc} or {};
      in acc // { ${svc} = mergeSvc prev entry; }
    ) {} mountsEnabled;

  servicesSanity =
    foldlAttrs (acc: name: m:
      if m.preconditionFile == null then acc else
      acc // {
        "nfs-mount-${name}-sanity-check" = {
          after = [ "nfs-mount-${name}-bindfs.service" ];
          wantedBy = [ "multi-user.target" ]
                     ++ lib.optional (m.bindToService != null)
                       "${m.bindToService}.service";
          serviceConfig.Type = "oneshot";
          script = ''
            stat "${m.mountPoint}/${m.preconditionFile}"
          '';
        };
      }
    ) {} mountsEnabled;

in {
  options.services.nfs-mount.mounts = mkOption {
    type = types.attrsOf (types.submodule nfsMountModule);
    default = {};
    description = ''
      A set of NFS mount definitions handled over WireGuard.
    '';
  };

  config = {
    inherit assertions;
    age.secrets."${host-id}-nfs-wireguard-key" = {
      generator.script = "wireguard-priv";
    };
    networking.hosts = networkingHosts;
    networking.wireguard.enable =
      lib.mkIf (mountsEnabled != {}) true;
    networking.wireguard.interfaces = wgInterfaces;
    systemd.mounts = mountsList;
    systemd.automounts = automountsList;
    bindfs-mappings.fqdns = bindfsMappings;
    systemd.services =
      servicesFromBinds // servicesSanity;
    users.groups = mapAttrs' (name: value: {
      name = value.bindToService;
      value = {};
    }) mountsEnabled;
  };
}
