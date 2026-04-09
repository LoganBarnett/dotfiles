################################################################################
# Central registry for service volumes under /tank/data.
#
# Declaring a volume creates the standard three-level directory layout and
# optionally generates a PostgreSQL dump service that runs before each
# Restic backup.
################################################################################
{
  lib,
  config,
  pkgs,
  ...
}:
let
  inherit (lib)
    concatLists
    filterAttrs
    mapAttrs'
    mapAttrsToList
    mkOption
    nameValuePair
    optional
    types
    ;
  cfg = config.tankVolumes;
  realPath = "/tank/data";
  volPath = name: "${realPath}/${name}";
  exportVols = filterAttrs (_: v: v.pgDatabase != null) cfg.volumes;
in
{
  options.tankVolumes = {
    volumes = mkOption {
      type = types.attrsOf (
        types.submodule {
          options = {
            pgDatabase = mkOption {
              type = types.nullOr types.str;
              default = null;
              description = ''
                PostgreSQL database name to dump before each Restic backup.
                When set, a tank-export-<name> oneshot service is generated
                that runs pg_dump as the postgres user and writes to
                /tank/data/<name>/exports/postgres.sql.
              '';
            };

            backupData = mkOption {
              type = types.bool;
              default = true;
              description = ''
                Whether to include this volume in the Restic backup.  Set to
                false for volumes that are too large or not worth preserving
                (e.g., media libraries populated from other sources).
              '';
            };

            group = mkOption {
              type = types.nullOr types.str;
              default = null;
              description = ''
                Group that owns the volume root and data/ directories.  Null
                uses root ownership.  When gid is also set, the group is
                created by this module; otherwise it is assumed to be created
                by another module (e.g., a NixOS service module).
              '';
            };

            gid = mkOption {
              type = types.nullOr types.int;
              default = null;
              description = ''
                GID to use when creating the group.  Required only when the
                group is not created by another module.  Omit for groups
                managed by a NixOS service module (e.g., gitea, postgresql).
              '';
            };
          };
        }
      );
      default = { };
      description = ''
        Volumes managed under /tank/data.  Each entry creates the volume
        root, data/, and exports/ subdirectories, and optionally registers a
        PostgreSQL dump service.
      '';
    };

    backupPaths = mkOption {
      type = types.listOf types.str;
      readOnly = true;
      description = ''
        Computed Restic backup paths derived from all declared volumes.
        Consumed by nfs-mount-provider's Restic backup job.
      '';
    };

    exportServiceNames = mkOption {
      type = types.listOf types.str;
      readOnly = true;
      description = ''
        Names of all generated tank-export-* systemd services.  Used by
        nfs-mount-provider to declare After/Wants ordering for the Restic
        backup job.
      '';
    };
  };

  config = {
    tankVolumes.backupPaths = concatLists (
      mapAttrsToList (name: v: optional v.backupData (volPath name)) cfg.volumes
    );

    tankVolumes.exportServiceNames = map (name: "tank-export-${name}.service") (
      lib.attrNames exportVols
    );

    systemd.tmpfiles.rules = concatLists (
      mapAttrsToList (
        name: v:
        let
          grp = if v.group != null then v.group else "root";
          exportGrp = if v.pgDatabase != null then "postgres" else "root";
          # When a PG export is configured, the postgres user needs to
          # traverse the volume root to reach exports/.  Grant o+x so
          # postgres can descend without being able to list or read.
          volMode = if v.pgDatabase != null then "0771" else "0770";
        in
        [
          "d ${volPath name}                   ${volMode} root ${grp}       -"
          "f ${volPath name}/nfs-working-share 0555 root root         -"
          "d ${volPath name}/data              0770 root ${grp}       -"
          "d ${volPath name}/exports           0770 root ${exportGrp} -"
        ]
      ) cfg.volumes
    );

    # Only create groups not managed by another module.  A non-null gid
    # indicates the group has no other owner in the NixOS module set.
    users.groups = mapAttrs' (name: v: nameValuePair v.group { gid = v.gid; }) (
      filterAttrs (_: v: v.group != null && v.gid != null) cfg.volumes
    );

    # One export service per volume with a pgDatabase configured.  Peer
    # authentication means no credentials are required.
    systemd.services = mapAttrs' (
      name: v:
      nameValuePair "tank-export-${name}" {
        description = "Export ${name} PostgreSQL database to ${volPath name}/exports/postgres.sql";
        serviceConfig = {
          Type = "oneshot";
          User = "postgres";
          ExecStart = "${config.services.postgresql.package}/bin/pg_dump --dbname=${v.pgDatabase} --file=${volPath name}/exports/postgres.sql";
        };
        after = [
          "postgresql.service"
          "tank-data.mount"
        ];
        requires = [ "postgresql.service" ];
      }
    ) exportVols;
  };
}
