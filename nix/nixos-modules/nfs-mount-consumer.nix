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
  toMountUnit = path: let
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
    norm = normalize p;
    # Escape literal "-" inside a path component (they must become \x2d).
    escapeDash = s: builtins.replaceStrings [ "-" ] [ "\\x2d" ] s;

    # Split by "/", drop empty bits (handles accidental //), escape, then join
    # with "-".
    comps =
      lib.filter (s: s != "") (lib.splitString "/" norm);
    stem =
      lib.concatStringsSep "-" (map escapeDash comps);
  in
    if p == "/"
    then "-"
    else stem;
  # Derive a systemd .mount unit name from a path, like `systemd-escape --path
  # --suffix=mount`.
  cfg = config.services.nfs-mount;
  mountsEnabled = lib.filterAttrs (_: m: m.enable or false) cfg.mounts;
  rawWhereOf = m: "${toString m.mountPoint}-raw";
  rawUnitOf  = m: "${toMountUnit (rawWhereOf m)}";
  peerIPv4Of = m: builtins.elemAt
    (builtins.match "([^/]*)/.*" m.wgPeerIP)
    0
  ;
  nfsMountModule = { name, config, ... }: let
    defaultInterface = "wgnfs-${config.vpnHost}";
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

    preconditionFile = mkOption {
      type = types.str;
      default = "nfs-working-share";
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
  ;

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
            name       = "nfs-${m.wgInterfaceName}";
            publicKey  = builtins.readFile m.wgPeerPublicKeyFile; # public only
            endpoint   = "${m.remoteHost}:${toString m.wgPort}";
            allowedIPs = [ m.wgPeerIP ];
            persistentKeepalive = 25;
          }];
        };
      }
    ) {} mountsEnabled;

  automountsList =
    mapAttrsToList (_: m: {
      # where = m.mountPoint;
      where = rawWhereOf m;
      after = [ "network-online.target" ];
      requires = [ "network-online.target" ];
      automountConfig.TimeoutIdleSec = "600";
    }) mountsEnabled;

  fileSystemsRaw =
    foldlAttrs (acc: name: m:
      acc // {
        "${rawWhereOf m}" = {
          device = "${m.vpnHost}:${m.share}";
          fsType = "nfs";
          options = [
            "_netdev"
            "addr=${peerIPv4Of m}"
            "x-systemd.automount"
            "noauto"
            "x-systemd.idle-timeout=600s"
            "x-systemd.requires=network-online.target"
            "x-systemd.after=network-online.target"
            "x-systemd.requires=wireguard-${m.wgInterfaceName}.service"
            "x-systemd.after=wireguard-${m.wgInterfaceName}.service"
            "nfsvers=4.2"
            "proto=tcp"
            "hard"
            "timeo=600"
            "retrans=3"
          ];
        };
      }
    ) {} mountsEnabled;

  # ----- bindfs-mappings (attrset) for your other module
  bindfsMappings =
    mapAttrs
      (name: m: {
        enable = true;
        source = rawWhereOf m;
        target = m.mountPoint;
        createForGroup = true;
        group = m.bindToService;
      })
      mountsEnabled
  ;
  bindfsSystemdConfigs =
    mapAttrs'
      (name: m: let
        after = [
          "${toMountUnit (rawWhereOf m)}.automount"
        ];
      in {
        name = "bindfs-${name}";
        value = {
          inherit after;
          requires = after;
        };
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
            "${rawUnitOf m}.mount"
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

  tmpfilesRules =
      # Lock the raw path used by NFS.
    (mapAttrsToList
      (_: m: "d ${rawWhereOf m} 000 root root - -")
      mountsEnabled
    )
      # And the final path that bindfs will present to services.
    ++ (mapAttrsToList
      (_: m: "d ${toString m.mountPoint} 000 root root - -")
      mountsEnabled
    );

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
    boot.supportedFilesystems = [ "nfs" ];
    environment.systemPackages = [
      # Allow us to debug NFS issues.
      pkgs.nfs-utils
      # Allow us to debug Wireguard issues.
      pkgs.wireguard-tools
    ];
    networking.hosts = networkingHosts;
    networking.wireguard.enable =
      lib.mkIf (mountsEnabled != {}) true;
    networking.wireguard.interfaces = wgInterfaces;
    # A gotcha: Don't use fileSystems and systemd.mounts.  fileSystems will emit
    # systemd.mounts, in effect.
    fileSystems = fileSystemsRaw;
    systemd.automounts = automountsList;
    bindfs-mappings.mounts = bindfsMappings;
    systemd.services =
      servicesFromBinds // servicesSanity // bindfsSystemdConfigs;
    users.groups = mapAttrs' (name: value: {
      name = value.bindToService;
      value = {};
    }) mountsEnabled;
  };
}
