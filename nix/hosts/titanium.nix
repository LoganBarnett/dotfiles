{ disko-proper, flake-inputs }: { modulesPath, ... }: let
  host-id = "titanium";
  system = "x86_64-linux";
in {
  imports = [
    ../nixos-modules/amd-gpu.nix
    ../nixos-modules/lvm-uefi-disk.nix
    ../nixos-modules/steam-gaming.nix
    ../nixos-modules/uefi-systemd-boot.nix
    ../nixos-modules/x-desktop.nix
    (import ../nixos-modules/server-host.nix {
      inherit host-id flake-inputs system;
    })
    ({ config, lib, pkgs, ... }: {
      imports = [
        (modulesPath + "/installer/scan/not-detected.nix")
      ];
      boot.kernelModules = [
        "kvm-intel"
      ];
      boot.initrd = {
        availableKernelModules = [
          "ahci"
          "nvme"
          "sd_mod"
          "usb_storage"
          "usbhid"
          "xhci_pci"
        ];
        kernelModules = [
          "nvme"
          "dm-snapshot"
        ];
      };
      hardware.cpu.intel.updateMicrocode = lib.mkDefault
        config.hardware.enableRedistributableFirmware;
      # Enables DHCP on each ethernet and wireless interface. In case of
      # scripted networking (the default) this is the recommended approach. When
      # using systemd-networkd it's still possible to use this option, but it's
      # recommended to use it in conjunction with explicit per-interface
      # declarations with `networking.interfaces.<interface>.useDHCP`.
      networking.useDHCP = lib.mkDefault true;
      # networking.interfaces.enp5s0.useDHCP = lib.mkDefault true;
      nixpkgs.hostPlatform = system;
    })
  ];
}
