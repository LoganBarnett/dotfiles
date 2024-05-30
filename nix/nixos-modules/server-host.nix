################################################################################
# Configuration common to all hosts that are servers.
################################################################################
{
  disko-proper,
  flake-inputs,
  host-id,
}: { ... }: {
  imports = [
    {
      # Hostname is not an FQDN.
      networking.hostName = host-id;
      nixpkgs.overlays = (import ../overlays/default.nix);
      system.stateVersion = "23.11";
    }
    # We can't use `disko` because it's taken, I guess.
    disko-proper.nixosModules.disko
    (import ./secrets.nix {
      inherit flake-inputs host-id;
      host-public-key-file = ../secrets/${host-id}/initrd_host_ed25519_key.pub;
    })
    (import ./tls-leaf-proton.nix {
      inherit host-id;
    })
    ./server-host-pub-key.nix
    ./nix-flakes.nix
    ./nix-store-optimize.nix
    ./sshd.nix
    ./tls-trust.nix
    ./user-can-admin.nix
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
}
