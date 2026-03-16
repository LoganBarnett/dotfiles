{
  config,
  lib,
  pkgs,
  modulesPath,
  specialArgs,
  ...
}: let
  boot-mode-settings = {
    systemd-boot = {...}: {
      boot = {
        loader.efi.canTouchEfiVariables = true;
        loader.systemd-boot = {
          enable = true;
          # Default is true for backwards compatibility, but the recommended is
          # to set to false.
          # But setting it to true allows editing the startup options using "e"
          # which is very helpful for debugging.
          editor = true;
          memtest86.enable = true;
        };
      };
    };
    grub-legacy = { ... }: {
      boot = {
        # loader.grub.device = lib.mkDefault "/dev/sda";
        # If grub continues to give us grief, switch to systemd-boot.
        loader.grub = {
          enable = true;
          # device = "/dev/disk/by-id/ata-ST2000DX001-SSHD-8GB_Z4Z5REMV";
          device = "/dev/vda";
          efiSupport = false;
          # efiInstallAsRemovable = true;
        };
        loader.efi.canTouchEfiVariables = false;
      };
    };
    grub-efi = { ... }: {
      boot = {
        # loader.grub.device = lib.mkDefault "/dev/sda";
        # If grub continues to give us grief, switch to systemd-boot.
        loader.grub = {
          enable = false;
          # Ugh, we need to use this when the image is being made, but the other
          # for the machine.  I need to do some Nix gymnastics for this one.
          # device = "/dev/vda";
          # device = "/dev/disk/by-id/ata-ST2000DX001-SSHD-8GB_Z4Z5REMV";
          # device = "/dev/disk/by-id/wwn-0x5000c50091b1805c";
          # Use this for already-bootable machines I guess.
          device = "nodev";
          # device = "/dev/vda";
          efiSupport = true;
          useOSProber = true;
          efiInstallAsRemovable = true;
          # efiInstallAsRemovable = false;
        };
        loader.efi = {
          # canTouchEfiVariables = true;
          # efiSysMountPoint = "/boot";
        };
        # Grub itself is not a boot loader.  That's why we must also have
        # systemd.
        loader.systemd-boot = {
          enable = false;
          # Default is true for backwards compatibility, but the recommended is
          # to set to false.
          editor = false;
          memtest86.enable = true;
        };
      };
      # fileSystems."/" = {
      #   device = "/dev/disk/by-id/ata-ST2000DX001-SSHD-8GB_Z4Z5REMV";
      #   fsType = "ext4";
      # };
      # Taken from
      # https://github.com/ksevelyar/idempotent-desktop/blob/6569a758601bd748cfb2b86d939dcd7eae29a3bf/hosts/tv.nix#L101
      # as it looks very promising.
      fileSystems."/" = {
        device = "/dev/disk/by-label/nixos";
        autoResize = true;
        fsType = "ext4";
        options = ["noatime" "nodiratime"];
      };

      fileSystems."/boot" = {
        # neededForBoot = true;
        # device = "/dev/disk/by-label/boot";
        device = "/dev/disk/by-label/ESP";
        fsType = "vfat";
        options = ["noatime" "nodiratime"];
      };
      # Pure cargo culting.
      # boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
      # More cargo culting.
      # boot.tmp.cleanOnBoot = true;
      # boot.tmp.useTmpfs = true;
    };
  };
  boot-mode = "systemd-boot";
in {
  imports = [
    boot-mode-settings.${boot-mode}
  ];
  boot = {
    growPartition = true;
    # There is advice saying to use this, but I haven't found this to be
    # universally applicable.  Sometimes the "S" needs to be omitted.  When this
    # should be done, and if there is a default that can intelligently do this,
    # I do not know.
    # kernelParams = ["console=ttyS0"];
    kernelParams = ["console=tty0"];
    loader.timeout = lib.mkDefault 0;
    initrd.availableKernelModules = ["uas"];
  };
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/nixos";
      autoResize = true;
      fsType = "ext4";
      options = ["noatime" "nodiratime"];
    };
    "/boot" = {
      # neededForBoot = true;
      # device = "/dev/disk/by-label/boot";
      device = "/dev/disk/by-label/ESP";
      fsType = "vfat";
      options = ["noatime" "nodiratime"];
    };
  };
  system.build.raw = import "${toString modulesPath}/../lib/make-disk-image.nix" {
    inherit lib config pkgs;
    additionalSpace = "0M";
    diskSize = specialArgs.diskSize or "auto";
    installBootLoader = true;
    # partitionTableType = "legacy";
    partitionTableType = "efi";
    # partitionTableType = "efixbootldr";
    touchEFIVars = true;
    format = "raw";
  };
}
