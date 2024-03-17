################################################################################
# This defines the entirety of the configuration for the lithium host.
#
# The lithium host is currently tasked as a machine learning server.  It has a
# very beefy profile for a typical server, as it has served a former life as a
# gaming rig.
#
# The lithium host uses BIOS despite the BIOS being littered with "UEFI"
# verbiage.
#
# The final NixOS module is the server specific configuration, with everything
# before that being reusable modules (or parameterized, reusable modules).
################################################################################
{ diskoProper, nixos-hardware }: let
  system = "x86_64-linux";
in {
  inherit system;
  specialArgs = {
    inherit nixos-hardware;
  };
  modules = [
    # We can't use `disko` because it's taken, I guess.
    diskoProper.nixosModules.disko
    ../users/logan-server.nix
    ../nixos-modules/nix-flakes.nix
    ../nixos-modules/nix-store-optimize.nix
    ../nixos-modules/nvidia.nix
    ../nixos-modules/sshd.nix
    ../nixos-modules/comfyui-server.nix
    ({ lib, ... }: {
      disko.devices = {
        disk.disk1 = {
          device = lib.mkDefault "/dev/sda";
          type = "disk";
          content = {
            # Required for MBR.  Despite the "BIOS" (or whatever the
            # confirguration is for the motherboard) saying "UEFI", it's either
            # in a compatibility mode that I can't figure out how to change, or
            # is flat out incorrect.  Use `sudo parted /dev/sda` to load that
            # disk, and then `p` to inspect it.
            type = "gpt"; # Grub Partition Table?
            partitions = {
              boot = {
                name = "boot";
                size = "1M";
                type = "EF02"; # 02 is Grub's MBR.
                # type = "EF00"; # 02 is UEFI.
              };
              # Why is this here in the example?  Leaving this out seems to make
              # a GPT+BIOS boot actually work.
              # esp = {
              #   name = "ESP";
              #   size = "500M";
              #   type = "EF00";
              #   content = {
              #     type = "filesystem";
              #     format = "vfat";
              #     mountpoint = "/boot";
              #   };
              # };
              root = {
                name = "root";
                size = "100%";
                content = {
                  type = "lvm_pv";
                  vg = "pool";
                };
              };
            };
          };
        };
        # TODO: It would be good to put a swap partition in place.
        lvm_vg = {
          pool = {
            type = "lvm_vg";
            lvs = {
              root = {
                size = "100%FREE";
                content = {
                  type = "filesystem";
                  format = "ext4";
                  mountpoint = "/";
                  mountOptions = [
                    "defaults"
                  ];
                };
              };
            };
          };
        };
      };
    })
    ({ pkgs, ... }: {
      # I don't know what else to set this to that's meaningful, but it has to
      # be set to _something_.  This is very likely something that will bite me
      # later.
      # boot.loader.grub.devices = [ "/dev/sda1" ];
      # Or maybe this is required to get BIOS booting working?
      boot.loader.grub.devices = [ "nodev" ];
      boot.loader.grub.enable = true;
      # boot.loader.grub.enable = false;
      # boot.loader.efi.canTouchEfiVariables = true;
      # boot.loader.efi.efiSysMountPoint = "/boot";
      # boot.loader.systemd-boot.enable = true;
      # This is just blindly copied from somewhere, but I don't know where.  I
      # should audit them in my Vast Quantities of Space Time™.
      boot.initrd.availableKernelModules = [
        "xhci_pci"
        "nvme"
        # There's no thunderbolt ports, so see about removing this.
        "thunderbolt"
        "usb_storage"
        "usbhid"
        "sd_mod"
      ];
      # Are all of these needed?  Perhaps just to _read_ these filesystems, such
      # as from a USB drive?  I certainly don't use all of these in partitions.
      boot.supportedFilesystems = [
        "btrfs"
        "ext2"
        "ext3"
        "ext4"
        "exfat"
        "f2fs"
        "fat8"
        "fat16"
        "fat32"
        "ntfs"
        "xfs"
      ];
      environment.systemPackages = [
        # Strangely, virtually required because of the odd way in which Nix
        # Flakes copies the configuration to the Nix store.
        pkgs.git
        # So we can run a janky session with the server, until it gets better
        # operationalized.
        pkgs.tmux
        # A base requirement for projectile searching with Emacs, but also a
        # handy too.
        pkgs.ripgrep
        # The only valid rsync destinations and sources are hosts that have
        # rsync installed.
        pkgs.rsync
      ];
      # Hostname is not an FQDN.
      networking.hostName = "lithium";
      system.stateVersion = "23.11";
    })
  ];
}
