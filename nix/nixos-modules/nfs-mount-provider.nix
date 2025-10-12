################################################################################
# Provide NFS shared volumes. Volumes are secured via WireGuard in conjunction
# with IP-specific access from NFS.
################################################################################
# nixos-modules/nfs-provider.nix
{ lib, config, host-id, pkgs, ... }:
let
  inherit (lib) mkIf mkEnableOption mkOption types concatStringsSep all unique;
  cfg = config.nfsProvider;
  # We use a btrfs snapshot to prevent write locks from fouling up the
  # backup.  This is the real path we want, which used to go into `paths`.
  realPath = "/tank/data";
  snapshotDir = "${realPath}/snapshots";
  # WireGuard interface/name and provider address are fixed by convention.
  wgIface = "wgnfs0";
  providerCidr = "10.100.0.1/24";
  wgSecretName = "${host-id}-nfs-wireguard-key";
  # Helpers.
  volAbsPath = v: "${realPath}/${v.volumeRelativeDir}";

  # Derived collections.
  volPaths = map volAbsPath cfg.volumes;

  exportsText =
    let
      line = v:
        "${volAbsPath v} 10.100.0.${toString v.peerNumber}/32" +
        "(rw,sync,no_subtree_check,no_root_squash)";
    in concatStringsSep "\n" (map line cfg.volumes);
  borgPatterns = (map (v: "+ ${v.volumeRelativeDir}/**") cfg.volumes)
                 ++ [ "- *" ];
in
{
  options.nfsProvider = {
    enable = mkEnableOption ''
      Enable the NFS provider: ensures a group exists, enforces directory
      ownership and permissions, configures a WireGuard interface (name is
      fixed to wgnfs0), opens the firewall, enables NFSv4, and exports each
      volume to its mapped peer IP 10.100.0.<peerNumber>/32. A Borg backup
      of all volumes runs nightly at 03:00 (10:00 UTC).
    '';

    volumes = mkOption {
      type = types.listOf (types.submodule (_: {
        options = {
          hostId = mkOption {
            type = types.str;
            description = ''
              Identifier for the consumer host. Used to locate the peer public
              key at "../secrets/<hostId>-nfs-wireguard-key.pub". Not required
              to be unique.
            '';
            example = "nextcloud";
          };

          gid = mkOption {
            type = types.int;
            description = ''
              The GID to use for the group.  Try to use an arbitrarily high one
              to avoid collisions (>= 29970).
            '';
            example = 29970;
          };

          group = mkOption {
            type = types.str;
            description = ''
              Group that will own each exported directory. Directories are
              created with mode 0770 and group ownership set to this group.
            '';
            example = "nextcloud";
          };
          volumeRelativeDir = mkOption {
            type = types.str;
            description = ''
              Directory name under ${realPath}. The exported path will be
              "${realPath}/<volumeRelativeDir>".
            '';
            example = "gitea";
          };
          peerNumber = mkOption {
            type = types.ints.u8;
            description = ''
              Last octet of the WireGuard peer's IP on 10.100.0.0/24. The peer
              is granted NFS access as 10.100.0.<peerNumber>/32. For example,
              5 gives 10.100.0.5/32.
            '';
            example = 4;
          };
        };
      }));
      default = [ ];
      description = ''
        List of volumes, each mapped to a WireGuard peer. Each item provides a
        hostId for key lookup, a subdirectory under ${realPath}, and a peer
        number for the 10.100.0.0/24 subnet.
      '';
      example = [
        {
          hostId = "copper";
          peerNumber = 3;
          volumeRelativeDir = "nextcloud";
          group = "nextcloud";
          gid = 29971;
        }
        {
          hostId = "copper";
          peerNumber = 3;
          volumeRelativeDir = "gitea";
          group = "gitea";
          gid = 29972;
        }
      ];
    };

  };

  config = mkIf cfg.enable {
    age.secrets."${host-id}-borg-encryption-passphrase" = {
      generator.script = "long-passphrase";
    };

    #### Group and directories
    users.groups = lib.pipe cfg.volumes [
      (builtins.map (v: {
        name = v.group;
        value = { gid = v.gid; };
      }))
      builtins.listToAttrs
    ];

    systemd.tmpfiles.rules =
      lib.lists.concatMap (v: let
        absoluteVolumePath = volAbsPath v;
      in [
        "d ${absoluteVolumePath} 0770 root ${v.group} -"
        # Include this as a sanity check, which the consumer can be configured
        # to look for via `preconditionCheck`.
        "f ${absoluteVolumePath}/nfs-working-share 0555 root root -"
      ]) cfg.volumes;

    #### Provider WireGuard private key via agenix-rekey
    age.secrets.${wgSecretName} = {
      generator.script = "wireguard-priv";
      mode = "0400";
      owner = "root";
    };

    #### Firewall
    networking.firewall = {
      interfaces.${wgIface}.allowedTCPPorts = [ 2049 ];
      allowedUDPPorts = [ 51820 ];
    };

    #### WireGuard interface + peers derived from volumes
    networking.wireguard.interfaces.${wgIface} = {
      ips = [ providerCidr ];
      listenPort = 51820;
      privateKeyFile = config.age.secrets.${wgSecretName}.path;
      peers =
        let
          groups = lib.groupBy (v: v.hostId) cfg.volumes;
          # One peer per hostId; take the first entry for pubkey + peerNumber.
          toPeer = vs:
            let v0 = builtins.head vs;
            in {
              publicKey =
                builtins.readFile
                  ../secrets/generated/${v0.hostId}-nfs-wireguard-key.pub;
              allowedIPs = [ "10.100.0.${toString v0.peerNumber}/32" ];
            };
        in map toPeer (builtins.attrValues groups);
    };

    #### NFS server and exports restricted to each peer /32
    services.nfs.server = {
      enable = true;
      exports = exportsText;
    };

    #### Borg backup covering all volumes (nightly at 03:00 / 10:00 UTC)
    # Borg runs as root by default, so we don't need to mess with permissions
    # until we wish to secure this further.
    services.borgbackup.jobs.nfsProvider = let
      btrfsBin = "${pkgs.btrfs-progs}/bin/btrfs";
    in {
      # We know we'll always be backing up from the same relative place, so we
      # don't need to use the "/" path for absolutes.
      paths = [ "${snapshotDir}/data" ];
      patterns = borgPatterns;
      # The jobs are disabled from writing anywhere but a select few
      # directories.  Since we need to create a snapshot, let's bless the
      # snapshot directory.
      readWritePaths = [ snapshotDir ];
      repo = "/tank/backup/backup-repo";
      encryption.mode = "repokey-blake2";
      encryption.passCommand = "cat ${
        config.age.secrets."${host-id}-borg-encryption-passphrase".path
      }";
      compression = "zstd";
      prune.keep = { daily = 7; weekly = 4; monthly = 6; };
      startAt = "10:00";

      preHook = ''
        set -euo pipefail
        SNAP_NAME="data-$(date +%Y%m%d-%H%M%S)"
        SNAP_PATH="${snapshotDir}/$SNAP_NAME"
        mkdir --parents "$SNAP_PATH"
        # -r (read-only) has no long form in current btrfs-progs.
        ${btrfsBin} subvolume snapshot -r ${realPath} "$SNAP_PATH"
        ln -sfn "$SNAP_PATH" ${snapshotDir}/data
      '';
      postHook = ''
        set -euo pipefail
        rm -f ${snapshotDir}/data
        ${btrfsBin} subvolume delete "$SNAP_PATH"/data
      '';
    };

    #### Guardrails
    assertions = [
      {
        assertion = cfg.volumes != [ ];
        message = "nfsProvider.volumes must not be empty.";
      }
      {
        assertion = all (n: n > 0 && n < 255)
          (map (v: v.peerNumber) cfg.volumes);
        message = "Each peerNumber must be between 1 and 254.";
      }
      {
        assertion = unique (map (v: v.volumeRelativeDir) cfg.volumes)
          == (map (v: v.volumeRelativeDir) cfg.volumes);
        message = "volumeRelativeDir values must be unique.";
      }
      (let
        # Ensure string keys for groupBy
        groups = lib.groupBy (v: toString v.hostId) cfg.volumes;

        analyzed =
        lib.mapAttrsToList (hostId: vs:
          let
            nums = map (v: v.peerNumber) vs;
            uniq = lib.unique nums;
          in {
            inherit hostId nums uniq;
            ok = (lib.length uniq) == 1;
          }
        ) groups;

      bad = lib.filter (g: !g.ok) analyzed;
      in {
        assertion =
          bad == [];

        message = ''
          For each hostId, all peerNumber values must be identical across
          volumes.
          Failures (per hostId): ${builtins.toJSON bad}
        '';
      })
    ];
  };
}
