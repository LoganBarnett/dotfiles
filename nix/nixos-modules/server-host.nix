################################################################################
# Configuration common to all hosts that are servers.
################################################################################
{
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
    (import ./secrets.nix {
      inherit flake-inputs host-id;
    })
    (import ./tls-leaf-proton.nix {
      inherit host-id;
    })
    # A server should never sleep/suspend unless we have a really good reason.
    ./narcolepsy.nix
    ./nix-builder-provide.nix
    (import ./nix-flake-environment.nix {
      inherit (flake-inputs) nix nixpkgs programsdb;
    })
    ./nix-store-optimize.nix
    # Haven't gotten this working yet.
    # ./server-host-pub-key.nix
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
    # I believe this is leading to this error I see when transferring 500GB
    # files:
    # ssh_dispatch_run_fatal: Connection to 192.168.254.38 port 22: message authentication code incorrect
    # This is while using a lighter weight MAC (-o MACs=umac-64-etm@openssh.com)
    # per: https://dentarg.blog/post/186913288147/umac-64-etm
    # pkgs.openssh_hpn
    pkgs.openssh
  ];
  # This seems to cause build issues with users-groups.json for reasons that are
  # unclear.  Disable.
  # Wipes passwords, so don't use.
  users.mutableUsers = true;
  # Needed to build large dependencies, which can come from surprising places.
  # Without this, oom-killer will still kill g++ on 32GB (29GB free) hosts.
  swapDevices = [{
    device = "/swapfile";
    size = 16 * 1024; # 16GB.
  }];
}
