################################################################################
# Build provider export list from shared facts. Feeds nfsProvider.volumes by
# mapping facts.nfsVolumes into { hostId, volumeRelativeDir, peerNumber }.
################################################################################
{ lib, config, facts, ... }:
let
  inherit (lib) mkOption mkIf types mkEnableOption;

  cfg = config.nfsProviderFacts;

  # Map facts rows into provider module entries.
  toProviderVol = v: {
    hostId = v."host-id";
    volumeRelativeDir = v.volume;
    peerNumber = v.peerNumber;
    backupContents = v.backupContents or true;
  };
in
{
  options.nfsProviderFacts = {
    enable = mkEnableOption ''
      Enable generation of nfsProvider.volumes from facts.nfsVolumes.
    '';

    # Allow overriding group if desired; defaults match provider module.
    groupName = mkOption {
      type = types.str;
      default = "borg";
      description = ''
        Group that will own each exported directory. Directories are created
        with mode 0770 and group ownership set to this group.
      '';
      example = "borg";
    };
  };

  config = mkIf cfg.enable {
    nfsProvider = {
      enable = true;
      groupName = cfg.groupName;

      # Feed the provider module directly.
      volumes = map toProviderVol facts.nfsVolumes;
    };
  };
}
