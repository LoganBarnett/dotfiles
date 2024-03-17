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
{ disko, nixos-generators, nixpkgs, ... }@inputs: let
  system = "x86_64-linux";
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    config.nvidia.acceptLicense = true;
    _module.args = {
      diskoProper = disko;
      disko = disko;
      flake-inputs = inputs;
    };
    specialArgs.flake-inputs = inputs;
    specialArgs.diskoProper = disko;
    specialArgs.disko = disko;
    # See https://nixos.wiki/wiki/Linux_kernel for values and options.
    linux-packages = pkgs.linuxPackages_latest;
  };
in nixpkgs.lib.nixosSystem {
  inherit system;
  modules = [
    ../users/logan-server.nix
    ../nixos-modules/nix-flakes.nix
    ../nixos-modules/nix-store-optimize.nix
    ../nixos-modules/sshd.nix
    # We can't use `disko` because it's taken, I guess.
    pkgs.diskoProper.nixosModules.disko
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
    {
      boot.kernelPackages = pkgs.linux-packages;
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
        # Allow us to get the PCI Bus ID for the graphics card.  This will
        # render a litle differently than lspci, and nvidiaBusId demands a
        # specific format that's closer to what lshw puts out.  There might be a
        # flag that would fix it, but I've yet to find it.
        pkgs.lshw
        # The wiki is also wrong about this package - it doesn't exist.
        # pkgs.nvidia-persistenced
        # pkgs.nvidia-settings
        # pkgs.nvidia-x11
        # Somehow this executable is available to some folks for diagnostic
        # purposes, but this package apparently doesn't exist.
        # pkgs.nvidia-smi
      ];
      hardware.opengl.enable = true;
      hardware.opengl.driSupport = true;
      hardware.opengl.driSupport32Bit = true;
      # Most of this can be found here:
      # https://nixos.wiki/wiki/Nvidia#Nvidia_PRIME
      hardware.nvidia = {
        package = pkgs.linux-packages.nvidiaPackages.beta;
        # Actually this expression is broken.  The wiki is wrong?  Error:
        # error: attribute 'boot' missing
        # package = config.boot.kernelPackages.nvidiaPackages.production;
        # Modesetting is required.
        # Enable to fix screen tearing
        modesetting.enable = true;
        # Use the NVidia open source kernel module (not to be confused with the
        # independent third-party "nouveau" open source driver).
        # Support is limited to the Turing and later architectures. Full list of
        # supported GPUs is at:
        # https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus
        # Only available from driver 515.43.04+
        # Currently alpha-quality/buggy, so false is currently the recommended setting.
        open = false;
        # Enable the Nvidia settings menu,
        # accessible via `nvidia-settings`.
        nvidiaSettings = true;
        # Prime is setup with offload
        # Taken from: https://wiki.archlinux.org/title/Hybrid_graphics
        # Hybrid-graphics is a concept involving two graphics cards on same
        # computer. Laptop manufacturers have developed technologies involving
        # two graphic cards with different abilities and power consumption on a
        # single computer.
        # prime.offload.enable = true;
        # Bus ID of the NVIDIA GPU. You can find it using lspci, either under 3D
        # or VGA.  Examples in the wild will show "PCI:1:0:0" but that's not the
        # actual output of lspci.
        prime.nvidiaBusId = "PCI:1:0:0";
      };
      # I'd like to better understand what these are doing.
      imports = [
        pkgs.flake-inputs.nixos-hardware.nixosModules.common-pc
        pkgs.flake-inputs.nixos-hardware.nixosModules.common-pc-ssd
        pkgs.flake-inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
        pkgs.flake-inputs.nixos-hardware.nixosModules.common-gpu-nvidia-nonprime
      ];
      nixpkgs.config.allowUnfree = true;
      nixpkgs.config.nvidia.acceptLicense = true;
      # TODO: Use this to bless specific packages.
      # nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
      #   "roon-server"
      #   "vscode"
      # ];
      services.xserver.videoDrivers = [ "nvidia" ];
    }
  ];
}
