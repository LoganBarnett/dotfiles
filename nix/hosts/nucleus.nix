################################################################################
# The installer for NixOS.  The installer is a minimal NixOS configuration
# itself to be copied to a bootable drive.
#
# It gets its own unique hostname so we can treat it like its own machine.  This
# allows us to bootstrap a machine with a running NixOS (from a detachable
# drive), and then run bin/remote-deploy to apply the real settings to the
# machine.  This way we have a consistent hostname for whatever machine is being
# stood up.  It gives us a launching point with which to apply things like
# disko to create our partitions for us.
################################################################################
{
  flake-inputs,
  host-id,
  system,
  ...
} : let
in {
  imports = [
    # (import "${flake-inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix")
    # Per the NixOS documentation:
    # Provide an initial copy of the NixOS channel so that the user doesn't need
    # to run "nix-channel --update" first.
    # (import "${flake-inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installer-cd-dvd-channel.nix")
    # (import "${flake-inputs.nixpkgs}/nixos/modules/installer/cd-dvd/channel.nix")
    ../hacks/installer/installation-cd-minimal.nix
    # ../hacks/installer/cd-dvd-channel.nix
    ../nixos-modules/server-host.nix
    ({ pkgs, lib, ... }: {
      # Make it so we can read the USB device that we booted from.  Otherwise
      # stuff just doesn't work and we get a "timed out waiting for device"
      # error.  This seems to have no effect on the build though, and is
      # probably included via the CD installer module.
      boot.initrd.availableKernelModules = [ "uas" "usbcore" "usb_storage" ];
      # I don't know what else to set this to that's meaningful, but it has to
      # be set to _something_.  This is very likely something that will bite me
      # later.
      # boot.loader.grub.devices = [ "/dev/sda1" ];
      # Or maybe this is required to get BIOS booting working?
      # Verify: This solves the dreaded "unable to mount root fs on unknown
      # block(0, 0)" error, which many elude to being a different problem.
      # iso-image.nix handles the grub menu I guess.
      # boot.loader.grub.devices = [ "nodev" ];
      # iso-image.nix handles the grub menu I guess.
      # boot.loader.grub.enable = true;
      environment.systemPackages = [
        # Use to setup partitions on the disk.
        # (pkgs.callPackage ./bootstrap/bootstrap-disk.nix)
        # This gives us nixos-config, which is needed if we want to make changes
        # to the installer after we have flashed it somewhere.  Otherwise we
        # cannot run `nixos-rebuild switch`.
        pkgs.nixos-install-tools
        # Use git so we can use nix flakes.
        pkgs.git
        # rsync must be installed on the destination to do syncs.
        pkgs.rsync
        # Give us vim in case we need to make some quick edits.
        pkgs.vim
      ];
      # Disable building documentation.  We don't it, we don't need to build it,
      # and it tends to cause cross-compilation issues.
      documentation.enable = lib.mkForce false;
      documentation.man.enable = lib.mkForce true;
      documentation.nixos.enable = lib.mkForce false;
      documentation.doc.enable = lib.mkForce false;
      documentation.info.enable = lib.mkForce false;
      # New changes - need to be tested to see if they help with the boot
      # problem.
      # Two modules are fighting over this value.  We want the boot loader, so
      # just force it.
      # - /nixos/modules/system/boot/loader/generic-extlinux-compatible/default.nix
      # - /nixos/modules/system/boot/loader/systemd-boot/systemd-boot.nix
      # system.build.installBootLoader = lib.mkForce true;
      boot.loader.systemd-boot.enable = true;
      boot.loader.efi.canTouchEfiVariables = false;
      boot.loader.grub.efiInstallAsRemovable = true;
      # fileSystems."/" = {
      #   device = "/dev/disk/by-label/NIXOS_SD";
      #   fsType = "ext4";
      # };
      # fileSystems."/boot" = {
      #   device = "/dev/disk/by-label/ESP";
      #   fsType = "vfat";
      # };
      # Force it to be FAT32 to help make this work against recalcitrant UEFI
      # implementations.
      # fileSystems."/boot".format = lib.mkForce "FAT32";
      # fileSystems."/boot" = lib.mkForce {
      #   device = "/dev/disk/by-label/ESP";
      #   fsType = "vfat";
      #   options = [ "shortname=winnt" "utf8" ];
      # };
      # isoImage.efiBootPartitionSize = 100;
      isoImage.makeUsbBootable = lib.mkForce true;
      isoImage.makeBiosBootable = lib.mkForce true;
      isoImage.makeEfiBootable = lib.mkForce true;
      # isoImage.squashfsCompression = "xz -Xdict-size 100% -Xbcj x86";
      # virtualisation.graphics = false;
      # Hostname is not an FQDN.
      networking.hostName = "nucleus";
      nixpkgs.hostPlatform = system;
      # I verified that setting overlays in the pkgs creation in flake.nix
      # does not work for making the overlays stick.  This does, however.
      # Probably because this module is getting a different pkgs due to
      # cross-compilation.
      nixpkgs.overlays = [
        (final: prev: {
          # makeDBusConf = { suidHelper, serviceDirectories, apparmor ? "disabled" }:
          #   prev.callPackage ../hacks/make-dbus-conf/make-dbus-conf.nix {
          #     inherit suidHelper serviceDirectories apparmor;
          #   };
          # nixos-configuration-reference-manpage =
          #   abort builtins.traceVerbose "nucleus-configuration overlay for nixos-configuration-reference-manpage"
          #     prev.stdenv.mkDerivation {
          #       name = "nixos-configuration-reference-manpage";
          #     };
          # documentation =
          #   builtins.traceVerbose "nucleus-configuration overlay for documentation"
          #     prev.documentation.overrideAttrs {
          #       baseOptionsJSON = null;
          #     };
        })
      ];
      services.openssh.settings.PasswordAuthentication = lib.mkOverride 50 true;
      # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
      system.stateVersion = "23.11";
    })
  ];
}
