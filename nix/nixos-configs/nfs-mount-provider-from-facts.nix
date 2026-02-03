################################################################################
# Use `../facts.nix` to declare `nfsProvider` volumes.
#
# This module can safely be included in all hosts, even if they don't have NFS
# volumes to expose.
#
# Presently, the borg backup jobs run as root.  We could use a DynamicUser there
# and then set the user to be a member of all of the groups used here.
################################################################################
{ facts, host-id, ... }: {
  imports = [
    ../nixos-modules/nfs-mount-provider.nix
  ];

  nfsProvider = let
    ownedVolumes = builtins.filter
      (v: v.providerHostId == host-id)
      facts.network.nfsVolumes
      ;
  in {
    enable = builtins.length ownedVolumes != 0;
    volumes = builtins.map
      (v: {
        hostId = v.consumerHostId;
        volumeRelativeDir = v.volume;
        inherit (v) gid group peerNumber;
        backupContents = v.backupContents or true;
      })
      ownedVolumes
    ;
  };

}
