################################################################################
# Configuration common to all hosts that are servers.
################################################################################
{ flake-inputs, host-id, lib, pkgs, system, ... }: {
  imports = [
    {
      # Hostname is not an FQDN.
      networking.hostName = host-id;
      nixpkgs.hostPlatform = system;
      nixpkgs.overlays = (import ../overlays/default.nix {
        inherit flake-inputs system;
      });
      system.stateVersion = "23.11";
      documentation.enable = lib.mkForce false;
    }
    # {
    #   age.rekey = {
    #     # TODO: This is the host key, and we should call it that instead of the
    #     # pub key.  The .pub is the pub key, but we also have a private key and
    #     # having that called the pub-key doesn't make sense.  Make sure to
    #     # capture other references, and rename what's already on disk.
    #     hostPubkey = ../secrets/${host-id}-pub-key.pub;
    #   };
    # }
    ./environment-file-secrets.nix
    ./lib-custom.nix
    ./nested-submodule-config-proof.nix
    ../nixos-configs/networking-static.nix
    # TODO: Test this - I think I put this in to solve remote build issues, but
    # I don't know if it actually did anything.
    # {
    #   nix = {
    #     settings = {
    #       extra-trusted-users = [ "logan" ];
    #     };
    #   };
    # }
    ./https-module.nix
    ./secrets.nix
    ./facts-secrets.nix
    # Allow servers to consume builds from other hosts.
    ./nix-builder-consume.nix
    # TODO: Remove this and only include it on hosts that need it.  Also make it
    # use the domain.
    ./tls-leaf-proton.nix
    # A server should never sleep/suspend unless we have a really good reason.
    ./narcolepsy.nix
    # See facts.nix for how this is specifically configured per host.
    ../nixos-modules/prometheus-client.nix
    (import ./nix-flake-environment.nix {
      inherit (flake-inputs) nix nixpkgs programsdb;
    })
    ./nix-store-optimize.nix
    # Haven't gotten this working yet.
    # ./server-host-pub-key.nix
    ./sshd.nix
    ./tls-trust.nix
    ./user-can-admin.nix
    ./user-lockout-schedule.nix
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
    # This gives us strings among other things.
    pkgs.binutils
    (pkgs.callPackage ../packages/ethernet-restart.nix {})
    # Show us details about a file.
    pkgs.file
    # Gives us ldapsearch et. al. for debugging LDAP issues.
    pkgs.openldap
    # Allow us to debug TLS issues.
    pkgs.openssl
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
    # Gives us lsusb which allows us to query USB devices.
    pkgs.usbutils
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
  # Further make life easier for builds by lowering the OOM score of the service
  # used to build.
  systemd.services.nix-daemon.serviceConfig = {
    OOMPolicy = "continue";
    OOMScoreAdjust = -1000;
  };
}
