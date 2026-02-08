################################################################################
# This defines the entirety of the configuration for the silicon host.
################################################################################
{ config, lib, flake-inputs, host-id, pkgs, system, ... }: let
in {
  imports = [
    ../nixos-modules/makemkv-ripper.nix
    ../nixos-modules/makemkv-updater.nix
    ../nixos-modules/nix-builder-provide.nix
    ../nixos-modules/server-host.nix
    ../nixos-configs/blocky-with-updater.nix
    ../nixos-configs/dns-smart-block.nix
    ../nixos-configs/dns-smart-block-admin.nix
    ../nixos-configs/grafana-kiosk-overview.nix
    ../nixos-configs/nfs-mount-provider-from-facts.nix
    flake-inputs.dns-smart-block.nixosModules.default
    # TODO: Right now agenix-rekey wants to build wireguard to do the
    # generation.  This fails due to a problem with macOS building wireguard-go
    # (documented in the overlay in this repository).  It is not understood why
    # it's not using the overlays.  I've removed the root nixpkgs channel and it
    # still provides this error, even with all the settings see in
    # ../nixos-modules/nix-flake-environment.nix.  I'm at a loss here.  This
    # probably warrants a ticket and I'll get to that sometime.  I have
    # confirmed that commenting this out (at least temporarily) fixes the issue.
    # However I already have pre-generated secrets so I'm not bumping into the
    # issue currently.
    ../nixos-modules/wireguard-server-standard.nix
    ({ pkgs, ... }: {
      # networking.hostId is needed by the filesystem stuffs.
      # An arbitrary ID needed for zfs so a pool isn't accidentally imported on
      # a wrong machine (I'm not even sure what that means).  See
      # https://search.nixos.org/options?channel=24.05&show=networking.hostId&from=0&size=50&sort=relevance&type=packages&query=networking.hostId
      # for docs.
      # Get from an existing machine using:
      # head -c 8 /etc/machine-id
      # Generate for a new machine using:
      # head -c4 /dev/urandom | od -A none -t x4 | tr -d ' '
      networking.hostId = "d88d6477";
      nixpkgs.hostPlatform = system;
    })
    # Silicon has a main disk for the OS, and then two disks for data.   One for
    # the primary data and one as an incremental backup.
    {
      imports = [
        flake-inputs.disko.nixosModules.default
      ];
      # Very relevant to disko configuration, since not having these settings
      # will cause the grub installation to fail.
      # boot.loader.systemd-boot.enable = true;
      boot.loader.systemd-boot.enable = false;
      # boot.loader.efi.canTouchEfiVariables = true;
      # boot.loader.grub = {
      #   device = "nodev";
      #   efiSupport = true;
      #   # efiInstallAsRemovable = true;
      # };
      boot.loader.grub = {
        enable = true;
        # devices = [ "/dev/disk/by-id/ata-WDC_WD80EAAZ-00BXBB0_WD-RD2G0Z2H" ];
        # device = "/dev/disk/by-id/ata-WDC_WD80EAAZ-00BXBB0_WD-RD2G0Z2H";
        # device = "nodev";
        devices = [ "nodev" ];
        # Force it to respect the device declaration sibling to this.
        # forceInstall = true;
        efiSupport = false;
      };
      disko.devices = {
        disk = {
          os = {
            type = "disk";
            device = "/dev/disk/by-id/ata-WDC_WDS100T2B0A-00SM50_200616A00BEF";
            content = {
              type = "gpt";
              efiGptPartitionFirst = false;
              partitions = {
                grub = {
                  size = "1M";
                  # BIOS boot partition.
                  type = "EF02";
                  # Lowest.
                  priority = 0;
                };
                boot = {
                  size = "512M";
                  # Linux filesystem.
                  type = "8300";
                  content = {
                    type = "filesystem";
                    format = "ext4";
                    mountpoint = "/boot";
                  };
                };
                # Hybrid support, in case we want to try UEFI again.
                # ESP = {
                #   size = "1G";
                #   type = "EF00";
                #   content = {
                #     type = "filesystem";
                #     format = "vfat";
                #     mountpoint = "/boot";
                #     mountOptions = [ "umask=0077" ];
                #   };
                # };
                root = {
                  size = "100%";
                  content = {
                    type = "filesystem";
                    format = "btrfs";
                    mountpoint = "/";
                  };
                };
              };
            };
          };
          data = {
            type = "disk";
            device = "/dev/disk/by-id/ata-WDC_WD80EAAZ-00BXBB0_WD-RD2G0Z2H";
            content = {
              type = "gpt";
              partitions = {
                data = {
                  size = "100%";
                  content = {
                    type = "lvm_pv";
                    vg = "vgData";
                  };
                };
              };
            };
          };
          backup = {
            type = "disk";
            device = "/dev/disk/by-id/ata-WDC_WD80EAAZ-00BXBB0_WD-RD2EW11H";
            content = {
              type = "gpt";
              partitions = {
                backup = {
                  size = "100%";
                  content = {
                    type = "lvm_pv";
                    vg = "vgBackup";
                  };
                };
              };
            };
          };
        };
        lvm_vg = {
          vgData = {
            type = "lvm_vg";
            lvs = {
              lvData = {
                size = "100%FREE";
                content = {
                  type = "filesystem";
                  format = "btrfs";
                  mountpoint = "/tank/data";
                };
              };
            };
          };
          vgBackup = {
            type = "lvm_vg";
            lvs = {
              lvBackup = {
                size = "100%FREE";
                content = {
                  type = "filesystem";
                  format = "btrfs";
                  mountpoint = "/tank/backup";
                };
              };
            };
          };
        };
      };
    }
  ];

  # Configure ACLs for shared media directory so both nextcloud and kodi can
  # read/write.
  systemd.services.setup-shared-media-acls = {
    description = "Set up ACLs for shared media directory";
    wantedBy = [ "multi-user.target" ];
    after = [ "tank-data.mount" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    script = ''
      ${pkgs.acl}/bin/setfacl -d -m g:media-shared:rwx /tank/data/nextcloud-shared-media
      ${pkgs.acl}/bin/setfacl -m g:media-shared:rwx /tank/data/nextcloud-shared-media
    '';
  };

  # Btrfs deduplication for /tank/data only. Runs weekly to reclaim space from
  # duplicate files (e.g., photos re-uploaded by phones).
  environment.systemPackages = [ pkgs.duperemove ];

  systemd.services.duperemove-tank-data = {
    description = "Deduplicate files on /tank/data using duperemove";
    after = [ "tank-data.mount" ];
    serviceConfig = {
      Type = "oneshot";
      # Run with lower priority to avoid impacting other services.
      Nice = 19;
      IOSchedulingClass = "idle";
    };
    script = ''
      ${pkgs.duperemove}/bin/duperemove \
        -r \
        -d \
        --dedupe-options=same \
        --hashfile=/tank/data/.duperemove.hash \
        -h \
        /tank/data
    '';
  };

  systemd.timers.duperemove-tank-data = {
    description = "Weekly deduplication of /tank/data";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      # Run Sundays at 2:00 AM (before the 3:00 AM backup).
      OnCalendar = "Sun *-*-* 02:00:00";
      Persistent = true;
    };
  };

  # MakeMKV DVD ripper for Silicon's internal drive.
  services.makemkv-ripper = {
    enable = true;
    outputPath = "/tank/data/kodi-media";
    autoRip = true;
    ejectWhenComplete = true;
    outputUser = "root";
    outputGroup = "media-shared";
    defaultDevice = "/dev/sr0";
  };

  # Auto-update MakeMKV beta key monthly.
  services.makemkv-updater = {
    enable = true;
    user = "root";
    group = "root";
  };

  # Allow unfree MakeMKV package.
  allowUnfreePackagePredicates = [
    (pkg: builtins.elem (lib.getName pkg) [ "makemkv" ])
  ];

  # Silicon provides builds to other hosts, so it should not consume builds
  # from itself.  Override the nix-builder-consume module imported by
  # server-host.nix to prevent self-referential build loops.
  nix.distributedBuilds = lib.mkForce false;
  nix.buildMachines = lib.mkForce [];
}
