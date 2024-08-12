################################################################################
# Configuration common to all hosts that are servers.
################################################################################
{
  disko-proper,
  flake-inputs,
  host-id,
  system,
}: { pkgs, ... }: {
  imports = [
    {
      # Hostname is not an FQDN.
      networking.hostName = host-id;
      nixpkgs.overlays = (import ../overlays/default.nix);
      system.stateVersion = "23.11";
      nix = {
        nixPath = [ "/etc/nix/path" ];
        # Make sure we have a local copy of nixpkgs but it can also be updated
        # online.  See
        # https://discourse.nixos.org/t/problems-after-switching-to-flake-system/24093/7
        # for more details.
        registry.nixpkgs.flake = flake-inputs.nixpkgs;
      };
    }
    # We can't use `disko` because it's taken, I guess.
    disko-proper.nixosModules.disko
    (import ./secrets.nix {
      inherit flake-inputs host-id;
    })
    (import ./tls-leaf-proton.nix {
      inherit host-id;
    })
    ./nix-builder-provide.nix
    ./nix-flakes.nix
    ./nix-store-optimize.nix
    ./sshd.nix
    ./tls-trust.nix
    (import ./user-can-admin.nix {
      inherit flake-inputs system;
    })
    ../users/logan-server.nix
  ];
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
    # Should address potential speed concerns with scp/rsync transfers.
    # I'm not sure why this wouldn't be the default, and haven't found anything
    # to that effect yet.
    pkgs.openssh_hpn
  ];
}
