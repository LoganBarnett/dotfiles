##
# arsenic is Kai's gaming computer.
##
{ disko-proper, flake-inputs }: let
  host-id = "arsenic";
  system = "x86_64-linux";
in { modulesPath, ... }: {
  nixpkgs.hostPlatform = system;
  imports = [
    (import ../nixos-modules/nvidia.nix {
      inherit flake-inputs;
      bus-id = "PCI:c:0.0";
      # According to https://en.wikipedia.org/wiki/Pascal_(microarchitecture)
      # this only goes to 6.0, and this can cause build problems with
      # magma/ptxas later.  This
      # (https://en.wikipedia.org/wiki/CUDA#GPUs_supported) shows we can go to
      # ... 8.0 or 6.x?  I'm not sure how to read this chart.  The actual name
      # of the chip (from the product level that I understand it - a GTX 1060
      # 6GB) says 6.1.
      cudaCapabilities = [ "8.6" ];
    })
    (import ../nixos-modules/server-host.nix {
      inherit host-id flake-inputs system;
    })
    ../nixos-modules/lvm-uefi-disk.nix
    ../nixos-modules/steam-gaming.nix
    ../nixos-modules/uefi-systemd-boot.nix
    ../nixos-modules/x-desktop.nix
    ../users/cassandra-desktop.nix
    ../users/kai-desktop.nix
    ({ config, lib, pkgs, ... }: {
      nixpkgs.hostPlatform = system;
      nix.channel.enable = false;
    })
    # From nixos-generate-config:
    ({ config, lib, modulesPath, ... }: {
      imports = [
        (modulesPath + "/installer/scan/not-detected.nix")
      ];
      boot.initrd.availableKernelModules = [
        "ahci"
        "ehci_pci"
        "nvme"
        "rtsx_usb_sdmmc"
        "sd_mod"
        "usb_storage"
        "usbhid"
        "xhci_pci"
      ];
      # Should really be handled elsewhere, yeah?
      networking.useDHCP = lib.mkDefault true;
      boot.initrd.kernelModules = [
        "dm-snapshot"
        "nvme"
      ];
      boot.kernelModules = [
        "kvm-amd"
      ];
      boot.extraModulePackages = [];
      # This is AMD, not Intel...
      # hardware.cpu.intel.updateMicrocode =
      #   lib.mkDefault config.hardware.enableRedistributableFirmware;
      hardware.cpu.amd.updateMicrocode =
        lib.mkDefault config.hardware.enableRedistributableFirmware;
      swapDevices = [];
    })
    {
      hardware.graphics = {
        enable = true;
      };
    }
  ];
}