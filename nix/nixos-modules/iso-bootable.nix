{ disko-proper }: { lib, ... }: {
  imports = [
    # ../raw-image.nix
    # ../iso-image.nix
    # We can't use `disko` because it's taken, I guess.
    disko-proper.nixosModules.disko
  ];
  # boot.postBootCommands = ''
  #   ln -s /dev/sda1 /dev/root
  # '';
  # isoImage = {
  #   # Remove the "Install" suffix.
  #   appendToMenuLabel = "";
  #   # Remove the "Install" prefix.
  #   prependToMenuLabel = "";
  #   # EFI booting.
  #   makeEfiBootable = true;
  #   # USB booting.
  #   makeUsbBootable = true;
  #   # Much faster than xz.
  #   # squashfsCompression = lib.mkDefault "zstd";
  #   # Just disable this since it might be causing problems.
  #   # squashfsCompression = null;
  # };
  # fileSystems = {
  #   "/" = {
  #     device = "/dev/disk";
  #     fsType = "vfat";
  #   };
  # };
  disko.devices = {
    disk = {
      main = {
        # device = "/dev/disk/by-id/some-disk-id";
        device = "/dev/disk/by-id/ata-ST2000DX001-SSHD-8GB_Z4Z5REMV";
        # device = "/dev/disk/by-label/nixos";
        type = "disk";
        content = {
          type = "gpt";
          # Why this gets part2 and the other gets part1 is unknowable to mortal
          # beings.
          # device = "/dev/disk/by-id/ata-ST2000DX001-SSHD-8GB_Z4Z5REMV-part2";
          partitions = {
            ESP = {
              type = "EF00";
              # size = "256M";
              size = "500M";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
              };
            };
            root = {
              # device = "/dev/disk/by-id/ata-ST2000DX001-SSHD-8GB_Z4Z5REMV-part1";
              size = "100%";
              content = {
                type = "filesystem";
                format = "ext4";
                mountpoint = "/";
              };
            };
          };
        };
      };
    };
    # disk.disk1 = {
    #   device = lib.mkDefault "/dev/sda";
    #   type = "disk";
    #   content = {
    #     # Required for MBR.  Despite the "BIOS" (or whatever the
    #     # confirguration is for the motherboard) saying "UEFI", it's either
    #     # in a compatibility mode that I can't figure out how to change, or
    #     # is flat out incorrect.  Use `sudo parted /dev/sda` to load that
    #     # disk, and then `p` to inspect it.
    #     type = "gpt"; # Grub Partition Table?
    #     partitions = {
    #       boot = {
    #         name = "boot";
    #         size = "1M";
    #         type = "EF02"; # 02 is Grub's MBR.
    #         # type = "EF00"; # 02 is UEFI.
    #       };
    #       # Why is this here in the example?  Leaving this out seems to make
    #       # a GPT+BIOS boot actually work.
    #       # esp = {
    #       #   name = "ESP";
    #       #   size = "500M";
    #       #   type = "EF00";
    #       #   content = {
    #       #     type = "filesystem";
    #       #     format = "vfat";
    #       #     mountpoint = "/boot";
    #       #   };
    #       # };
    #       root = {
    #         name = "root";
    #         size = "100%";
    #         content = {
    #           type = "lvm_pv";
    #           vg = "pool";
    #         };
    #       };
    #     };
    #   };
    # };
    # lvm_vg = {
    #   pool = {
    #     type = "lvm_vg";
    #     lvs = {
    #       root = {
    #         size = "100%FREE";
    #         content = {
    #           type = "filesystem";
    #           format = "ext4";
    #           mountpoint = "/";
    #           mountOptions = [
    #             "defaults"
    #           ];
    #         };
    #       };
    #     };
    #   };
    # };
  };
  # boot.growPartition = true;
  # boot.loader.systemd-boot.enable = true;
}
