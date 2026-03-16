################################################################################
# Defines our default LVM partitions.
#
# I prefer LVM as a starting point since it's very flexible in terms of
# expanding disk space with physical disks but not having to segment partitions
# necessarily.
################################################################################
{ ... }: {
  fileSystems."/" = {
    autoResize = true;
    device = "/dev/pool/root";
    fsType = "ext4";
    options = ["noatime" "nodiratime"];
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-label/ESP";
    fsType = "vfat";
    options = ["noatime" "nodiratime"];
  };
}
