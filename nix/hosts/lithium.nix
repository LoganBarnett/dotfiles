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
{ disko-proper, flake-inputs, host-id, system, ... }: let
in {
  imports = [
    ../nixos-modules/bedtime-for-screen-addicts.nix
    ../users/solomon-desktop.nix
    (let
      # Default ComfyUI port.
      comfyui-port = 8188;
    in {
      imports = [
        # (import ../nixos-modules/comfyui-server.nix {
        #   inherit host-id;
        #   port = comfyui-port;
        # })
        # (import ../nixos-modules/https.nix {
        #   inherit host-id;
        #   listen-port = 443;
        #   server-port = comfyui-port;
        #   fqdn = "${host-id}.proton";
        # })
      ];
    })
    (import ../nixos-modules/nvidia.nix {
      inherit flake-inputs;
      bus-id = "PCI:1:0:0";
      # According to https://en.wikipedia.org/wiki/Pascal_(microarchitecture)
      # this only goes to 6.0, and this can cause build problems with
      # magma/ptxas later.  This
      # (https://en.wikipedia.org/wiki/CUDA#GPUs_supported) shows we can go to
      # ... 8.0 or 6.x?  I'm not sure how to read this chart.  The actual name
      # of the chip (from the product level that I understand it - a GTX 1060
      # 6GB) says 6.1.
      cudaCapabilities = [ "6.0" ];
    })
    ../nixos-modules/server-host.nix
    ({ lib, pkgs, ... }: {
      # Freaky workaround that's already fixed, but we're pinned to an older
      # nixpkgs.  See: https://github.com/NixOS/nixpkgs/issues/261777
      programs.fish.vendor.config.enable = false;
      # Override the value from ../nixos-modules/nix-flake-environment.nix
      # because we're on 23.05 for lithium to make nvidia support smoother.
      # nix.package = lib.mkForce flake-inputs.nix.packages.${pkgs.system}.nix;
      nix.settings = {
        # Allow building i686-linux binaries too.  This is helpful if an
        # x86_64-linux build needs 32bit support, which is what i686 indicates.
        extra-platforms = [
          "i686-linux"
        ];
      };
      nixpkgs.hostPlatform = system;
      nixpkgs.overlays = [
        (final: prev: {
          # pythonPackagesExtensions = [(py-final: py-prev: {
          #   tensorboard = py-prev.tensorboard.overrideAttrs (old: {
          #     # This could be made smarter.
          #     disabled = false;
          #   });
          # })];
        })
      ];
      # This is just to make lithium able to emit a Raspberry Pi image.  It is
      # not recommended since lithium is not an arm system.
      # boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
      # nix.settings = {
      #   trusted-substituters = [
      #     "https://raspberry-pi-nix.cachix.org"
      #   ];
      #   trusted-public-keys = [
      #     "raspberry-pi-nix.cachix.org-1:WmV2rdSangxW0rZjY/tBvBDSaNFQ3DyEQsVw8EvHn9o="
      #   ];
      # };
      # I suspect this is killing my builds of torch.  Stop that!
      systemd.oomd.enable = false;
    })
    ({ lib, ... }: {
      imports = [
        disko-proper.nixosModules.disko
      ];
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
    ({ pkgs, ... }: {

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
      # This was the only way I could get around this error:
      #
      # building '/nix/store/dvcd1s8gmpw2whz34cqmk1sxaws6mzjm-lazy-options.json.drv'...
      # error: builder for '/nix/store/dvcd1s8gmpw2whz34cqmk1sxaws6mzjm-lazy-options.json.drv' failed with exit code 1;
      #        last 10 log lines:
      #        >           957|   fixupOptionType = loc: opt:
      #        >              |                          ^
      #        >           958|     if opt.type.getSubModules or null == null
      #        >
      #        >        error: value is a function while a set was expected
      #        > Cacheable portion of option doc build failed.
      #        > Usually this means that an option attribute that ends up in documentation (eg `default` or `description`) depends on the restricted module arguments `config` or `pkgs`.
      #        >
      #        > Rebuild your configuration with `--show-trace` to find the offending location. Remove the references to restricted arguments (eg by escaping their antiquotations or adding a `defaultText`) or disable the sandboxed build for the failing module by setting `meta.buildDocsInSandbox = false`.
      #        >
      #        For full logs, run 'nix log /nix/store/dvcd1s8gmpw2whz34cqmk1sxaws6mzjm-lazy-options.json.drv'.
      #
      # I did not ask to build lazy-options, and it wasn't apparent why it was
      # needed.  The options there (printed via a debug version) show it's
      # normal to have functions there that are decorated for option-use.  No
      # option I have set seems to be causing this.  It's shown up in a somewhat
      # recent commit (sometime in November 2023, by my estimation), but I lack
      # the capability to do a proper bisect on it to find out.  There may be a
      # way to do it, but given how brittle torch is to build, I will leave this
      # to more eager minds.  This error should be much more helpful than it is,
      # and that is perhaps the _real_ bug here that I need to file for.
      documentation.enable = true;
    })
  ];
}
