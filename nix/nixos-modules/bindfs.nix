################################################################################
# Use bindfs to do some permission or uid/gid remapping of a mount.
################################################################################
{ config, lib, pkgs, ... }: let
  inherit (lib) mkMerge mkOption types;
  cfg = config.bindfs-mappings;
in {

  options.bindfs-mappings = mkOption {
    type = types.attrsOf (types.submodule ({ name, ... }: {
      options = {
        source = mkOption {
          type = types.str;
          description = "Source directory to bind";
        };
        target = mkOption {
          type = types.str;
          description = "Target mount point";
        };
        uid = {
          source = mkOption {
            type = types.int;
            description = "Original UID to map from";
          };
          destination = mkOption {
            type = types.int;
            description = "UID to map to";
          };
        };
        gid = {
          source = mkOption {
            type = types.nullOr types.int;
            default = null;
            description = "Original GID to map from (optional)";
          };
          destination = mkOption {
            type = types.nullOr types.int;
            default = null;
            description = "GID to map to (optional)";
          };
        };
        extraOptions = mkOption {
          type = types.listOf types.str;
          default = [];
          description = "Extra bindfs options";
        };
      };
    }));
    default = {};
    description = "Declarative bindfs UID/GID mappings.";
  };

  config = {
    systemd.services = lib.attrsets.mapAttrs'
      (name: opts:
        let
          uidMap = "--map=${toString opts.uid.source}/${toString opts.uid.destination}";
          gidMap =
            if opts.gid.source != null && opts.gid.destination != null
            then "--map=@${toString opts.gid.source}/@${toString opts.gid.destination}"
            else "";
          extra = lib.concatStringsSep " " opts.extraOptions;
        in {
          name = "bindfs-${name}";
          value = {
            description = "Bindfs mapping ${name}";
            after = [ "network.target" ];
            wantedBy = [ "multi-user.target" ];
            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;
              ExecStart = ''
                ${pkgs.bindfs}/bin/bindfs ${uidMap} ${gidMap} ${extra} \
                  ${opts.source} ${opts.target}
              '';
              ExecStop = "${pkgs.util-linux}/bin/umount ${opts.target}";
            };
          };
        }
      )
      cfg
    ;
    systemd.tmpfiles.rules = lib.mapAttrsToList
      (_name: opts: let
        uid = opts.uid.destination;
        gid = opts.gid.destination;
      in
        "d ${opts.target} 0770 ${toString uid} ${toString gid} - -"
      )
      cfg
    ;
  };
}
