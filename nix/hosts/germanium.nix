##
# Germanium is Solomon's gaming computer.
##
{ disko-proper, flake-inputs }: let
  host-id = "germanium";
  system = "x86_64-linux";
in { modulesPath, ... }: {
  nixpkgs.hostPlatform = system;
  # isoImage.makeEfiBootable = true;
  # isoImage.makeUsbBootable = true;
  imports = [
    ../raw-image.nix
    # (import ../nixos-modules/iso-bootable.nix {
    #   inherit disko-proper;
    # })
    # Is this really an installer?
    # It's not an sd-card either.
    # "${toString modulesPath}/installer/sd-card/sd-image.nix"
    (import ../nixos-modules/server-host.nix {
      inherit host-id flake-inputs system;
    })
    # ({...}: {
    #   sdImage.populateRootCommands = "";
    #   sdImage.populateFirmwareCommands = "";
    # })
    # ../nixos-modules/steam-gaming.nix
    ({ config, lib, pkgs, ... }: {
      nixpkgs.hostPlatform = system;
      # system.build.image = import "${flake-inputs.nixpkgs}/nixos/lib/make-disk-image.nix" {
      #   diskSize = 10000;
      #   format = "qcow2-compressed";
      #   installBootLoader = true;
      #   partitionTableType = "efi";
      #   inherit config lib pkgs;
      # };
      # nix.channel.enable = false;
      # nix.settings.nix-path = "nixpkgs=flake:nixpkgs";
      # boot.growPartition = true;
      # boot.loader.systemd-boot.enable = true;
      # not sure if needed
      # boot.initrd.kernelModules = [ "nvme" ];
      # boot.loader.grub = {
      #   efiSupport = true;
      #   efiInstallAsRemovable = true;
      #   device = "nodev";
      # };

      # fileSystems."/" = { device = "/dev/vda2"; fsType = "ext4"; };
      # fileSystems."/boot" = { device = "/dev/vda1"; fsType = "vfat"; };
      # system.stateVersion = "21.11";
    })
    # From nixos-generate-config:
    ({ config, lib, modulesPath, ... }: {
      imports = [
        (modulesPath + "/installer/scan/not-detected.nix")
      ];
      boot.initrd.availableKernelModules = [
        "xhci_pci"
        "ehci_pci"
        "ahci"
        "usb_storage"
        "usbhid"
        "sd_mod"
        # Unsure what this is supposed to be.  I can't find reference to it, and
        # attempts to build end with:
        # > root module: rtsxusbsdmmc
        # > modprobe: FATAL: Module rtsxusbsdmmc not found in directory /nix/store/kjkwva5h8zacpayc2pbyr98wai1nddk1-linux-6.6.34-modules/lib/modules/6.6.34
        # I may have typoed it from a hand-copy of nixos-generate-config, but
        # attempts to reconstruct the correct spelling have failed.  Not all
        # kernel modules exist in the nixpkgs repository.
        # "rtsxusbsdmmc"
        "rtsx_usb_sdmmc"
      ];
      # Should really be handled elsewhere, yeah?
      networking.useDHCP = lib.mkDefault true;
      boot.kernelModules = [];
      boot.extraModulePackages = [];
      hardware.cpu.intel.updateMicrocode =
        lib.mkDefault config.hardware.enableRedistributableFirmware;
    })
  ];
}
