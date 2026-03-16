################################################################################
# Declare our standard LVM layout on a bootable disk.
################################################################################
{ ... }: {
  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "xfs";
    options = ["noatime" "nodiratime"];
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-label/EFIBOOT";
    fsType = "vfat";
    options = ["noatime" "nodiratime"];
  };
}
