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
{ diskoProper, flake-inputs }: let
  # Default ComfyUI port.
  comfyui-port = 8188;
  host-id = "lithium";
  system = "x86_64-linux";
in {
  inherit system;
  specialArgs = {
    inherit flake-inputs;
  };
  modules = [
    (import ../nixos-modules/secrets.nix {
      inherit flake-inputs host-id system;
      host-public-key-file = ../secrets/lithium-ssh-key.pub;
    })
    # We can't use `disko` because it's taken, I guess.
    diskoProper.nixosModules.disko
    (import ../nixos-modules/https.nix {
      inherit host-id;
      listen-port = 443;
      server-port = comfyui-port;
      fqdn = "lithium.proton";
    })
    ../users/logan-server.nix
    ../nixos-modules/nix-flakes.nix
    ../nixos-modules/nix-store-optimize.nix
    ../nixos-modules/nvidia.nix
    ../nixos-modules/sshd.nix
    ../nixos-modules/user-can-admin.nix
    (import ../nixos-modules/comfyui-server.nix {
      inherit host-id;
      port = comfyui-port;
    })
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
    ({ config, pkgs, ... }: {

      age.secrets."tls-${host-id}" = {
        generator = {
          dependencies = [
            config.age.secrets.internal-ca
          ];
          script = "tls-signed-certificate";
        };
        settings = {
          root-certificate = config.age.secrets.internal-ca;
          fqdn = "lithium.proton";
        };
        rekeyFile = ../secrets/tls-${host-id}.age;
      };

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
      documentation.enable = false;
      environment.systemPackages = [
        # Strangely, virtually required because of the odd way in which Nix
        # Flakes copies the configuration to the Nix store.
        pkgs.git
        # Top, but prettier.
        pkgs.htop
        # Let us see ports and file handles held by processes.
        pkgs.lsof
        # So we can run a janky session with the server, until it gets better
        # operationalized.
        pkgs.tmux
        # A base requirement for projectile searching with Emacs, but also a
        # handy too.
        pkgs.ripgrep
        # The only valid rsync destinations and sources are hosts that have
        # rsync installed.
        pkgs.rsync
        # We need an editor.  Even if we don't want to edit configuration files
        # because we already have Nix, it's still helpful for things that use
        # EDTIOR, such as C-x C-e in the shell.  Can we just uninstall nano?
        pkgs.vim
      ];
      # Hostname is not an FQDN.
      networking.hostName = host-id;
      nixpkgs.overlays = (import ../overlays/default.nix);
      system.stateVersion = "23.11";
    })
  ];
}
