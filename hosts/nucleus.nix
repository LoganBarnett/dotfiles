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
  config,
  flake-inputs,
  host-id,
  lib,
  options,
  pkgs,
  system,
  ...
} : let
in {
  imports = [
    (import "${flake-inputs.nixpkgs}/nixos/modules/installer/cd-dvd/iso-image.nix")
    (import "${flake-inputs.nixpkgs}/nixos/modules/profiles/minimal.nix")
    (import "${flake-inputs.nixpkgs}/nixos/modules/profiles/base.nix")
    (import "${flake-inputs.nixpkgs}/nixos/modules/profiles/installation-device.nix")
    ../users/logan-server.nix
    ../nixos-modules/sshd.nix
    ../nixos-configs/nix-store-tools.nix
    ../nixos-modules/narcolepsy.nix
    ../nixos-modules/nix-flake-environment.nix
    ../nixos-modules/nix-store-optimize.nix
    ../nixos-modules/nix-builder-consume.nix
    ../nixos-modules/user-can-admin.nix
    ../nixos-modules/secrets.nix
  ];
  # Make it so we can read the USB device that we booted from.  Otherwise
  # stuff just doesn't work and we get a "timed out waiting for device"
  # error.
  boot.initrd.availableKernelModules = [ "uas" "usbcore" "usb_storage" ];

  # Enable all hardware support (from installation-cd-base).
  hardware.enableAllHardware = true;

  # Add terminus font for HiDPI displays (from installation-cd-base).
  console.packages = options.console.packages.default ++ [ pkgs.terminus_font ];

  environment.systemPackages = [
    pkgs.git
    # This gives us nixos-config, which is needed if we want to make changes
    # to the installer after we have flashed it somewhere.  Otherwise we
    # cannot run `nixos-rebuild switch`.
    pkgs.nixos-install-tools
    # We need this to actually do rsync (needed on both sides).
    pkgs.rsync
    # Give us vim in case we need to make some quick edits.
    pkgs.vim
    # Make disko available for partitioning operations.
    flake-inputs.disko.packages.${system}.disko
  ];

  # Disable building documentation.  We don't it, we don't need to build it,
  # and it tends to cause cross-compilation issues.
  documentation.enable = lib.mkForce false;
  documentation.man.enable = lib.mkOverride 500 true;
  documentation.nixos.enable = lib.mkForce false;
  documentation.doc.enable = lib.mkOverride 500 true;
  documentation.info.enable = lib.mkForce false;

  fonts.fontconfig.enable = lib.mkOverride 500 false;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = false;
  boot.loader.grub.efiInstallAsRemovable = true;
  boot.loader.grub.memtest86.enable = true;

  # An installation media cannot tolerate a host config defined file system
  # layout on a fresh machine, before it has been formatted.
  swapDevices = lib.mkImageMediaOverride [ ];
  fileSystems = lib.mkImageMediaOverride config.lib.isoFileSystems;
  boot.initrd.luks.devices = lib.mkImageMediaOverride { };

  boot.postBootCommands = ''
    for o in $(</proc/cmdline); do
      case "$o" in
        live.nixos.passwd=*)
          set -- $(IFS==; echo $o)
          echo "nixos:$2" | ${pkgs.shadow}/bin/chpasswd
          ;;
      esac
    done
  '';

  isoImage.makeUsbBootable = lib.mkForce true;
  isoImage.makeBiosBootable = lib.mkForce true;
  isoImage.makeEfiBootable = lib.mkForce true;
  isoImage.edition = lib.mkOverride 500 "minimal";

  # Hostname is not an FQDN.
  networking.hostName = "nucleus";
  nixpkgs.hostPlatform = system;

  nixpkgs.overlays = [
    (final: prev: {
    })
  ];
  services.openssh.settings.PasswordAuthentication = lib.mkOverride 50 true;
  # Settle a conflict with ../nixos-modules/sshd.nix.
  services.openssh.settings.PermitRootLogin = lib.mkOverride 50 "yes";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = lib.mkDefault lib.trivial.release;
}
