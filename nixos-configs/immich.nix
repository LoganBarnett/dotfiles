################################################################################
# Immich photo/video backup on silicon.  Media lives on the /tank/data LVM
# volume so the service must wait for that mount before starting.
################################################################################
{ ... }:
{
  imports = [ ../nixos-modules/immich.nix ];

  services.immich-host = {
    enable = true;
    mediaLocation = "/tank/data/immich";
    mountDependencies = [ "tank-data.mount" ];
  };
}
