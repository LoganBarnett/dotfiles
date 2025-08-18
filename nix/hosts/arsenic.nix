##
# arsenic is Kai's gaming computer.
##
{ flake-inputs, host-id, modulesPath, system, ... }: {
  nixpkgs.hostPlatform = system;
  imports = [
    # Spared for good behavior.
    # ../nixos-modules/bedtime-for-screen-addicts.nix
    ../nixos-modules/cpu-frequency.nix
    (import ../nixos-modules/nvidia.nix {
      inherit flake-inputs;
      bus-id = "PCI:c:0.0";
      # According to https://en.wikipedia.org/wiki/Pascal_(microarchitecture)
      # this only goes to 6.0, and this can cause build problems with
      # magma/ptxas later.  This
      # (https://en.wikipedia.org/wiki/CUDA#GPUs_supported) shows we can go to
      # ... 8.0 or 6.x?  I'm not sure how to read this chart.  The actual name
      # of the chip (from the product level that I understand it - an RTX 3070
      # 8GB) says 8.6.
      # GeForce RTX 3070
      cudaCapabilities = [ "8.6" ];
    })
    (import ../nixos-modules/https.nix {
      inherit host-id;
      listen-port = 443;
      server-port = 8080;
      fqdn = "${host-id}.proton";
    })
    ../nixos-modules/ollama.nix
    ../nixos-modules/server-host.nix
    ../nixos-modules/shutdown-halt.nix
    ../nixos-modules/lvm-uefi-disk.nix
    ../nixos-modules/steam-gaming.nix
    ../nixos-modules/timezone-pacific.nix
    ../nixos-modules/uefi-systemd-boot.nix
    ../nixos-modules/x-desktop.nix
    ../users/cassandra-desktop.nix
    ../users/kai-desktop.nix
    ({ config, lib, pkgs, ... }: {
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
