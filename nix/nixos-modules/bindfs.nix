################################################################################
# Use bindfs to present a mount with group-based ownership for DynamicUser.
#
# This is just kept for reference.
#
# I guess because it's a FUSE plugin and therefore runs in userspce, it's
# ridiculously slow for massive file usage, which is basically what we want this
# for.  Plus it's single threaded per mount.  That kind of makes it seem like a
# toy rather than something of real use (at least for my use case).  See
# `uidmapfs.nix` for a more heavy duty, kernel space equivalent.
################################################################################
{ config, lib, pkgs, ... }:
let
  inherit (lib) mkMerge mkOption mkEnableOption types concatStringsSep;
  cfg = config.bindfs-mappings;
in {
  options.bindfs-mappings.mounts = mkOption {
    type = types.attrsOf (types.submodule ({ name, ... }: {
      options = {
        enable = (mkEnableOption ''
          Enable this bindfs mapping.
        '') // { default = true; };

        source = mkOption {
          type = types.str;
          description = ''
            Source directory to bind (usually your raw NFS mount).
          '';
          example = "/mnt/nextcloud-raw";
        };

        target = mkOption {
          type = types.str;
          description = ''
            Target mount point for the bindfs view.
          '';
          example = "/mnt/nextcloud/data";
        };

        group = mkOption {
          type = types.str;
          description = ''
            Group that files/dirs should appear to be owned by in the bind view.
            Your consuming service should set
            `SupplementaryGroups=${"\"<this>\""}`.
          '';
          example = "nextcloud";
        };

        createForGroup = mkOption {
          type = types.bool;
          default = false;
          description = ''
            When true, pass --create-for-group=<group> so new files/dirs are
            created as the presented group.
          '';
        };

        extraOptions = mkOption {
          type = types.listOf types.str;
          default = [
            # Ensure group read/write/exec where sensible; no world access.
            "--perms=u+rwX,g+rwX,o-rwx"
          ];
          description = ''
            Extra bindfs options. Prefer long options. If a short option is
            unavoidable, add a comment explaining it.
          '';
          example = [
            "--xattr-none"
            # -n: do not complain about ownership mismatches (doc-only example).
            "-n"
          ];
        };
      };
    }));
    default = {};
    description = ''
      Declarative bindfs mappings keyed by a friendly name. Each entry forces a
      single group in the bind view so services with DynamicUser can access via
      SupplementaryGroups.
    '';
    example = {
      nextcloud = {
        source = "/mnt/nextcloud-raw";
        target = "/mnt/nextcloud/data";
        group = "nextcloud";
        createForGroup = true;
        extraOptions = [ "--xattr-none" ];
      };
    };
  };

  config = {
    systemd.services = lib.attrsets.mapAttrs'
      (name: opts:
        let
          forceGroup = "--force-group=${opts.group}";
          createG    = lib.optionalString opts.createForGroup
            "--create-for-group=${opts.group}";
          extra      = concatStringsSep " " opts.extraOptions;
        in {
          name = "bindfs-${name}";
          value = lib.mkIf opts.enable {
            description = "bindfs mapping ${name}";
            # Make sure network and the source mount are available first.
            after  = [ "network-online.target" ];
            wants  = [ "network-online.target" ];
            wantedBy = [ "multi-user.target" ];

            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;

              ExecStart = ''
                ${pkgs.bindfs}/bin/bindfs \
                  ${forceGroup} \
                  ${createG} \
                  ${extra} \
                  "${opts.source}" "${opts.target}"
              '';
              ExecStop = "${pkgs.util-linux}/bin/umount \"${opts.target}\"";
            };
            # Ensure systemd waits for these paths to be mountable.
            unitConfig = {
              requiresMountsFor = [ opts.source opts.target ];
            };
          };
        }
      )
      cfg.mounts;

    # Ensure target mount points exist with proper group & perms.
    systemd.tmpfiles.rules = lib.mapAttrsToList
      (_: opts:
        # 2000 = setgid to preserve group assignment.
        "d ${opts.target} 2770 root ${opts.group} - -"
      )
      cfg.mounts;
  };
}
