{ flake-inputs, modulesPath, pkgs, ... }: let
  host-id = "titanium";
  system = "x86_64-linux";
in {
  imports = [
    (let
      # Default ComfyUI port.
      comfyui-port = 8188;
    in {
      # Turn off until we can figure out how to get ROCM and torch to play nice.
      # imports = [
      #   (import ../nixos-modules/comfyui-server.nix {
      #     inherit host-id;
      #     port = comfyui-port;
      #   })
      #   (import ../nixos-modules/https.nix {
      #     inherit host-id;
      #     listen-port = 443;
      #     server-port = comfyui-port;
      #     fqdn = "${host-id}.proton";
      #   })
      # ];
    })
    flake-inputs.home-manager.nixosModules.home-manager
    ../users/logan-desktop.nix
    ../nixos-modules/amd-gpu.nix
    ../nixos-modules/discord.nix
    ../nixos-modules/gaming-android.nix
    ../nixos-modules/lutris-gaming.nix
    ../nixos-modules/lvm-uefi-disk.nix
    ../nixos-modules/steam-gaming.nix
    ../nixos-modules/timezone-pacific.nix
    ../nixos-modules/uefi-systemd-boot.nix
    ../nixos-modules/x-desktop.nix
    ../nixos-modules/server-host.nix
    ../nixos-configs/sunshine.nix
    ../nixos-configs/musicgpt-ui.nix
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
      # I'm not sure why this must be disabled explicitly.  Not doing this
      # causes `config.cudaSupport` in the ComfyUI package to fail.
      # nixpkgs.config.cudaSupport = false;
    })
    # ../users/eric-desktop.nix
  ];
  environment.systemPackages = [
    # Let's be able to consume media.
    pkgs.ffmpeg
    # Let's be able to consume media.
    pkgs.vlc
    # The built in browser is a little lacking.
    pkgs.firefox
    pkgs.musicgpt
  ];
}
