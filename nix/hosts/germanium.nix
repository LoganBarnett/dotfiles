##
# Germanium is Solomon's gaming computer.
##
{ disko-proper, flake-inputs }: let
  host-id = "germanium";
  system = "x86_64-linux";
in { ... }: {
  nixpkgs.hostPlatform = system;
  imports = [
    (import ../nixos-modules/server-host.nix {
      inherit host-id flake-inputs disko-proper system;
    })
    # ../nixos-modules/steam-gaming.nix
    ({ config, lib, pkgs, ... }: {
      system.build.image = import "${flake-inputs.nixpkgs}/nixos/lib/make-disk-image.nix" {
        diskSize = 10000;
        format = "qcow2-compressed";
        installBootLoader = true;
        partitionTableType = "efi";
        inherit config lib pkgs;
      };
      # nix.channel.enable = false;
      # nix.settings.nix-path = "nixpkgs=flake:nixpkgs";
      boot.growPartition = true;
      # not sure if needed
      boot.initrd.kernelModules = [ "nvme" ];
      boot.loader.grub = {
        efiSupport = true;
        efiInstallAsRemovable = true;
        device = "nodev";
      };

      fileSystems."/" = { device = "/dev/vda2"; fsType = "ext4"; };
      fileSystems."/boot" = { device = "/dev/vda1"; fsType = "vfat"; };
    })
  ];
}
