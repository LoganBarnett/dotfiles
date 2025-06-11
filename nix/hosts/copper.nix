################################################################################
# This defines the entirety of the configuration for the copper host.
#
# Trivia:  About 95% of the copper ever mined has been extracted since 1900.
#
# Copper is my Nextcloud host.  A microSD just couldn't hold up and Nextcloud is
# needs a beefier system to handle it.  There might be old references to
# gallium, but they are old as of 2025-06-07.
################################################################################
{ flake-inputs, host-id, system, ... }: let
in {
  imports = [
    ../nixos-modules/nix-builder-provide.nix
    ../nixos-modules/server-host.nix
    ../nixos-modules/nextcloud.nix
    ../nixos-modules/notes-sync.nix
    ({ lib, pkgs, ... }: {
      # networking.hostId is needed by the filesystem stuffs.
      # An arbitrary ID needed for zfs so a pool isn't accidentally imported on
      # a wrong machine (I'm not even sure what that means).  See
      # https://search.nixos.org/options?channel=24.05&show=networking.hostId&from=0&size=50&sort=relevance&type=packages&query=networking.hostId
      # for docs.
      # Get from an existing machine using:
      # head -c 8 /etc/machine-id
      # Generate for a new machine using:
      # head -c4 /dev/urandom | od -A none -t x4 | tr -d ' '
      networking.hostId = "b310d071";
      nixpkgs.hostPlatform = system;
    })
    ({
      imports = [
        flake-inputs.disko.nixosModules.default
      ];
      # Very relevant to disko configuration, since not having these settings
      # will cause the grub installation to fail.
      boot.loader.systemd-boot.enable = true;
      # boot.loader.systemd-boot.enable = false;
      boot.loader.efi.canTouchEfiVariables = true;
      boot.loader.grub = {
        device = "nodev";
        efiSupport = true;
        # efiInstallAsRemovable = true;
      };
      disko.devices = {
        disk = {
          os = {
            type = "disk";
            device = "/dev/disk/by-id/ata-WDC_WD6400AAKS-75A7B0_WD-WCASY1103354";
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
      # the security issue.  See
      # https://github.com/NixOS/nixpkgs/issues/279362#issuecomment-1913506090
      # for more discussion.
      fileSystems."/boot".options = [ "umask=0077" "defaults" ];
    })
  ];
}
