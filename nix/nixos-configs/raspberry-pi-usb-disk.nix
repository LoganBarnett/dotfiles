################################################################################
# When creating an image for a Raspberry Pi with then intention of using that
# disk via USB, it needs some special considerations to be considered bootable.
################################################################################
{ flake-inputs, lib, ... }: {
  imports = [
    # flake-inputs.disko.nixosModules.disko
  ];
  # boot.loader.raspberryPi = {
    # enable = true;
    # Leave as-is due to:
    # https://github.com/nix-community/raspberry-pi-nix/issues/16
    # uboot.enable = true;
  # };
  boot.initrd.availableKernelModules = [
    "sd_mod"
    # USB Attached SCSI (fast path for many SSD bridges).
    "uas"
    # USB mass storage.
    "usb_storage"
    # USB3 host on Pi 4/5 hubs.
    "xhci_pci"
  ];
  boot.kernelParams = [
    # Ensure logs on HDMI.
    "console=tty1"
  #   # Shows logs if you later attach a UART dongle.
  #   "console=serial0,115200"
    "systemd.debug_shell=1"
  #   "rootwait"
    "rootdelay=5"
    # Disable UAS for this JMicron bridge.
    # You can query for the manufacturer ID (first ID), and product ID (second
    # ID) via:
    #
    "usb-storage.quirks=152d:a578:u"
    # Avoid autosuspend surprises.
    "usbcore.autosuspend=-1"
  #   "systemd.log_level=debug"
  #   "systemd.log_target=console"
  #   "boot.shell_on_fail"
  #   # Try to keep the screen from blanking after a certain point.
    "video=HDMI-A-1:1024x768@60D"
    "fbcon=nodefer"
  ];
  # Don't include uas in initrd kernel modules, because it causes some kind of
  # panic.  Instead, we can load it after the kernel has undergone its init
  # already.  This helps us combat what I am told are faulty controllers
  # (generally JMicron).
  boot.kernelModules = [ "uas" ];
  # blacklistedKernelModules = [ "uas" ];
  # fileSystems."/" = lib.mkForce {
  #   device = "/dev/disk/by-label/NIXOS_SD";
  #   # This is needed to ensure we get to use btrfs, since sdImage from
  #   # raspberry-pi-nix wants to use something else.
  #   # fsType = "btrfs";
  #   # options = [ "subvol=@" "compress=zstd" ];
  # };
  # systemd.additionalUpstreamSystemUnits = [ "debug-shell.service" ];
  # fileSystems."/boot/firmware".neededForBoot = true;
  # fileSystems."/boot/firmware".device =
  #   lib.mkForce "/dev/disk/by-label/FIRMWARE";
  # boot.supportedFilesystems = [ "btrfs" ];
  # boot.initrd.supportedFilesystems = [ "btrfs" ];
  # # This enables a weekly "TRIM" operation.  Nothing is "trimmed" in terms of FS
  # # size.  Instead, unused blocks are marked as unused so the wear-leveling
  # # mechanisms in SSDs know what they can pick back up.
  # services.fstrim.enable = true;

  # disko.devices = {
  #   disk.usb = {
  #     type = "disk";
  #     device = "/dev/disk/by-id/usb-JMicron_JMS579_DD5641988396A-0:0";
  #     content = {
  #       type = "table";
  #       format = "msdos"; # Pi firmware prefers MBR/DOS.
  #       partitions = [
  #         {
  #           name = "boot";
  #           start = "1MiB";
  #           end = "513MiB";
  #           bootable = true;
  #           content = {
  #             type = "filesystem";
  #             # AKA FAT32.
  #             format = "vfat";
  #             # label = "BOOT";
  #             # mkfsOptions = [
  #             #   # Force FAT32.
  #             #   "-F" "32"
  #             #   # Use "BOOT" label.
  #             #   "-n" "BOOT"
  #             # ];
  #             # Where NixOS + Pi looks.
  #             mountpoint = "/boot/firmware";
  #           };
  #         }
  #         {
  #           name = "nixos";
  #           start = "513MiB";
  #           end = "100%";
  #           content = {
  #             type = "btrfs";
  #             # format = "btrfs";
  #             # label = "NIXOS";
  #             mountOptions = [ "compress=zstd" "ssd" "noatime" ];
  #             # btrfs only does snapshots of subvolumes and not arbitrary
  #             # directories, so we have to set these up in advance to allow
  #             # snapshotting and restoration in favorable ways.
  #             subvolumes = {
  #               "@".mountpoint     = "/";
  #               "@nix".mountpoint  = "/nix";
  #               "@var".mountpoint  = "/var";
  #               "@log".mountpoint  = "/var/log";
  #               "@home".mountpoint = "/home";
  #             };
  #           };
  #         }
  #       ];
  #     };
  #   };
  # };

}
