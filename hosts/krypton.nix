################################################################################
# Krypton is a noble gas discovered in 1898, known for its use in high-powered
# lamps and photography.
#
# The krypton host is a Mac Mini 7,1 (Late 2014) with an Intel Core i5-4260U
# (Haswell) CPU.
#
# This host runs Kodi in standalone kiosk mode for TV media playback, using
# NFS-mounted media from the silicon host.  It is plugged directly into the TV.
################################################################################
{ flake-inputs, host-id, pkgs, system, ... }: {
  imports = [
    (flake-inputs.nixos-hardware + "/apple/macmini")
    ../nixos-modules/server-host.nix
    ../nixos-configs/kodi-media-player.nix
    ({ lib, ... }: {
      boot.initrd.availableKernelModules = [
        "ahci"
        "xhci_pci"
        "usbhid"
        "usb_storage"
        "sd_mod"
      ];
      boot.kernelModules = [ "kvm-intel" ];
      # This hostId is needed by some filesystems like ZFS.
      # Generated using: head -c4 /dev/urandom | od -A none -t x4 | tr -d ' '
      # You'll want to regenerate this on the actual hardware.
      networking.hostId = "4ead70ae";
      nixpkgs.hostPlatform = system;
    })
    ({
      imports = [
        flake-inputs.disko.nixosModules.default
      ];
      # Very relevant to disko configuration, since not having these settings
      # will cause the grub installation to fail.
      boot.loader.systemd-boot.enable = true;
      boot.loader.efi.canTouchEfiVariables = true;
      boot.loader.grub = {
        device = "nodev";
        efiSupport = true;
      };
      disko.devices = {
        disk = {
          os = {
            type = "disk";
            device = "/dev/disk/by-id/ata-APPLE_HDD_HTS545050A7E362_TNS5193T2X33VH";
            content = {
              type = "gpt";
              partitions = {
                boot = {
                  size = "512M";
                  # EFI System Partition.
                  type = "EF00";
                  content = {
                    type = "filesystem";
                    format = "vfat";
                    mountpoint = "/boot";
                  };
                };
                lvm = {
                  size = "100%";
                  content = {
                    type = "lvm_pv";
                    vg = "vg0";
                  };
                };
              };
            };
          };
        };
        lvm_vg.vg0 = {
          type = "lvm_vg";
          lvs = {
            root = {
              size = "100%FREE";
              content = {
                type = "filesystem";
                format = "btrfs";
                mountpoint = "/";
              };
            };
          };
        };
      };
      # Works around the issue where the installer warns that the boot seed is
      # leaked.  You may still get the warning, but this should actually address
      # the security issue. See
      # https://github.com/NixOS/nixpkgs/issues/279362#issuecomment-1913506090
      # for more discussion.
      fileSystems."/boot".options = [ "umask=0077" "defaults" ];
    })
  ];

  # Additional packages useful for a media server.
  environment.systemPackages = [
    # Let's be able to work with media files if needed.
    pkgs.ffmpeg
    # Useful for debugging media issues.
    pkgs.mediainfo
  ];
}
