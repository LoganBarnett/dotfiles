################################################################################
# NVMe disks are great when used with a HAT for a Raspberry Pi.  We can boot off
# of them, and they won't die after a few months of writing logs (unlike SD
# cards).
################################################################################
{ flake-inputs, ... }: {
  imports = [
    flake-inputs.disko.nixosModules.disko
  ];
  disko.devices = {
    disk = {
      nvme0 = {
        # Unsure if this is stable, but let's go with it.
        device = "/dev/disk/by-id/nvme0n1";
        type = "disk";

        content = {
          type = "gpt";
          partitions = {

            firmware = {
              name = "firmware";
              # It needs to be sufficiently large for how NixOS keeps bootable
              # generations.
              size = "512M";
              # EFI System Partition (FAT32).
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot/firmware";
                mountOptions = [
                  "fmask=0022"
                  "dmask=0022"
                ];
              };
            };

            root = {
              name = "root";
              size = "100%";
              content = {
                type = "filesystem";
                format = "ext4";
                mountpoint = "/";
                mountOptions = [
                  # I'm told `relatime` is fine even for SSD/NVMe disks, and
                  # doesn't hit nearly as often as `atime`.
                  # "noatime"
                ];
              };
            };

          };
        };
      };
    };
  };
}
