{
  config,
  flake-inputs,
  host-id,
  modulesPath,
  pkgs,
  system,
  ...
}:
{
  imports = [
    ../users/logan-desktop.nix
    ../nixos-configs/proc-siding-worker.nix
    ../nixos-configs/sonify-health-titanium.nix
    ../nixos-configs/sonify-health-goss.nix
    ../nixos-configs/airplay-server.nix
    ../nixos-configs/amd-gpu.nix
    ../nixos-configs/discord.nix
    ../nixos-configs/gaming-android.nix
    ../nixos-configs/lutris-gaming.nix
    ../nixos-modules/lvm-uefi-disk.nix
    ../nixos-configs/garage-queue-worker.nix
    ../nixos-configs/ollama.nix
    ../nixos-configs/ollama-models-12gb-vram.nix
    ../nixos-configs/steam-gaming.nix
    ../nixos-configs/timezone-pacific.nix
    ../nixos-configs/uefi-systemd-boot.nix
    ../nixos-modules/x-desktop.nix
    ../nixos-modules/linux-host.nix
    ../nixos-configs/sunshine.nix
    # ../nixos-configs/musicgpt-ui.nix
    (
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {
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
        home-manager.users.logan = {
          imports = [
            ../home-configs/lutris-gaming.nix
          ];
        };
        hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
        # Enables DHCP on each ethernet and wireless interface. In case of
        # scripted networking (the default) this is the recommended
        # approach. When using systemd-networkd it's still possible to use this
        # option, but it's recommended to use it in conjunction with explicit
        # per-interface declarations with
        # `networking.interfaces.<interface>.useDHCP`.
        networking.useDHCP = lib.mkDefault true;
        # networking.interfaces.enp5s0.useDHCP = lib.mkDefault true;
        nixpkgs.hostPlatform = system;
      }
    )
  ];
  # musicgpt-ui is currently commented out; alias is pre-declared for
  # when the service is re-enabled.
  networking.dnsAliases = [ "musicgpt" ];
  services.garage-queue-worker.workers.ollama.settings.capabilities.scalars.vram_mb =
    12288;
  hardware.amdGpuCard.index = 1;
  services.proc-siding.settings.detector_cmd = "${config.services.proc-siding.package}/share/proc-siding/detectors/amd-gpu.sh --exclude-unit ollama.service";
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
