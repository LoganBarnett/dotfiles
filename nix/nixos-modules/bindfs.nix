################################################################################
# Use bindfs to present a mount as a different user/group.
################################################################################
{ config, lib, pkgs, ... }:
let
  inherit (lib) mkMerge mkOption mkEnableOption types concatStringsSep;
  cfg = config.bindfs-mappings;
in {
  options.bindfs-mappings = mkOption {
    type = types.attrsOf (types.submodule ({ name, ... }: {
      options = {
        enable = (mkEnableOption ''
          Enable this bindfs mapping.
        '') // { default = true; };

        source = mkOption {
          type = types.str;
          description = ''
            Source directory to bind. This is the original mount or path
            whose ownership view you want to change.
          '';
          example = "/mnt/nextcloud-raw";
        };

        target = mkOption {
          type = types.str;
          description = ''
            Target mount point. bindfs will mount here with the presented
            user/group.
          '';
          example = "/mnt/nextcloud/data";
        };

        user = mkOption {
          type = types.str;
          description = ''
            Username that files should appear to be owned by. This is passed
            to bindfs as --force-user.
          '';
          example = "nextcloud";
        };

        group = mkOption {
          type = types.str;
          description = ''
            Group name that files should appear to be owned by. This is
            passed to bindfs as --force-group.
          '';
          example = "nextcloud";
        };

        createForUser = mkOption {
          type = types.bool;
          default = false;
          description = ''
            When true, pass --create-for-user=<user> so newly created files/dirs
            are owned by the presented user.  Generally you will want this to be
            false to avoid surprising behavior.  It could cause issues with
            ACLs.
          '';
        };

        createForGroup = mkOption {
          type = types.bool;
          default = false;
          description = ''
            When true, pass --create-for-group=<group> so newly created
            files/dirs are owned by the presented group.  Generally you will
            want this to be false to avoid surprising behavior.  It could cause
            issues with ACLs.
          '';
        };

        extraOptions = mkOption {
          type = types.listOf types.str;
          default = [];
          description = ''
            Extra bindfs options. Use this for additional flags. Short
            options should be commented to explain their purpose.
          '';
          example = [
            "--perms=0640:a+D"
            # -n: do not complain about ownership mismatches (doc-only example)
            "-n"
          ];
        };
      };
    }));
    default = {};
    description = ''
      Declarative bindfs mappings keyed by a friendly name. Each entry
      presents the source directory as owned by the given user and group at
      the target mount point.
    '';
    example = {
      nextcloud = {
        enable = true;
        source = "/mnt/nextcloud-raw";
        target = "/mnt/nextcloud/data";
        user = "nextcloud";
        group = "nextcloud";
        createForUser = true;
        createForGroup = true;
        extraOptions = [ "--xattr-none" ];
      };
    };
  };

  config = {
    systemd.services = lib.attrsets.mapAttrs'
      (name: opts:
        let
          forceUser = "--force-user=${opts.user}";
          forceGroup = "--force-group=${opts.group}";
          createU = lib.optionalString opts.createForUser
            "--create-for-user=${opts.user}";
          createG = lib.optionalString opts.createForGroup
            "--create-for-group=${opts.group}";
          extra = concatStringsSep " " opts.extraOptions;
        in {
          name = "bindfs-${name}";
          value = lib.mkIf opts.enable {
            description = "bindfs mapping ${name}";
            after = [ "network.target" ];
            wantedBy = [ "multi-user.target" ];
            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;
              # NOTE: bindfs has no long option for -n; avoid short flags
              # unless you comment them in extraOptions.
              ExecStart = ''
                ${pkgs.bindfs}/bin/bindfs \
                  ${forceUser} \
                  ${forceGroup} \
                  ${createU} \
                  ${createG} \
                  ${extra} \
                  "${opts.source}" "${opts.target}"
              '';
              ExecStop = "${pkgs.util-linux}/bin/umount \"${opts.target}\"";
            };
          };
        }
      )
      cfg;

    # Ensure target mount points exist with presented ownership.
    systemd.tmpfiles.rules = lib.mapAttrsToList
      (_: opts:
        "d ${opts.target} 0770 ${opts.user} ${opts.group} - -"
      )
      cfg;
  };
}
