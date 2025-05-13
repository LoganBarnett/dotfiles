################################################################################
# This defines the entirety of the configuration for the silicon host.
################################################################################
{ config, flake-inputs, host-id, system, ... }: let
in {
  imports = [
    ../nixos-modules/nix-builder-provide.nix
    ../nixos-modules/server-host.nix
    (import ../nixos-modules/kiosk-url.nix {
      url = "https://grafana.proton/d/system-monitoring/system-monitoring?orgId=1&from=now-6h&to=now&timezone=America/Los_Angeles&kiosk=fullscreen&refresh=5s";
      browser = "chromium";
    })
    # TODO: Right now agenix-rekey wants to build wireguard to do the
    # generation.  This fails due to a problem with macOS building wireguard-go
    # (documented in the overlay in this repository).  It is not understood why
    # it's not using the overlays.  I've removed the root nixpkgs channel and it
    # still provides this error, even with all the settings see in
    # ../nixos-modules/nix-flake-environment.nix.  I'm at a loss here.  This
    # probably warrants a ticket and I'll get to that sometime.  I have
    # confirmed that commenting this out (at least temporarily) fixes the issue.
    # (import ../nixos-modules/wireguard-server.nix {
    #   inherit host-id;
    # })
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
    {
      imports = [
        ../nixos-modules/wireguard-agenix-rekey-generator.nix
      ];
      services.nfs.server = {
        enable = true;
        exports = ''
         /tank/backup 10.100.0.2/32(rw,sync,no_subtree_check,no_root_squash)
         /tank/data/nextcloud 10.100.0.3/32(rw,sync,no_subtree_check,no_root_squash)
         /tank/data/gitea 10.100.0.4/32(rw,sync,no_subtree_check,no_root_squash)
        '';
      };
      networking.firewall = {
        allowedTCPPorts = [ ];
        interfaces."wg0".allowedTCPPorts = [ 2049 ];
        allowedUDPPorts = [ 51820 ];
      };
      networking.wireguard.interfaces.wg0 = {
        ips = [ "10.100.0.1/24" ];
        listenPort = 51820;
        privateKeyFile = config.age.secrets."${host-id}-nfs-wireguard-key".path;
        # Use peers to control who can get to what.
        peers = [
          # Borg.
          # Nextcloud.
          {
            publicKey = builtins.readFile
              ../secrets/nextcloud-nfs-wireguard-key.pub;
            # Use IPs in services.nfs.server.exports (see above) for controlling
            # which service gets to see which subdirectory.
            allowedIPs = [ "10.100.0.3/32" ];
          }
        ];
      };
    }
    {
      # This goes on the client host (such as ../nixos-modules/nextcloud.nix).
      # age.secrets.nextcloud-wireguard-key = {
      #   generator.script = "wireguard-priv";
      #   rekeyFile = ../secrets/${host-id}-wireguard-key.age;
      # };
      age.secrets."${host-id}-nfs-wireguard-key" = {
        generator.script = "wireguard-priv";
        rekeyFile = ../secrets/${host-id}-nfs-wireguard-key.age;
      };
      age.secrets.borg-passphrase = {
        # See secrets.nix for where this is declared.
        generator.script = "long-passphrase";
        group = "borg";
        mode = "0440";
        rekeyFile = ../secrets/${host-id}-borg-encryption-passphrase.age;
      };
      services.borgbackup.jobs.dataBackup = {
        paths = [ "/tank/data" ];
        repo = "/mnt/backup/backup-repo";
        encryption.mode = "repokey-blake2";
        encryption.passCommand = "cat /run/agenix/borg-passphrase";
        compression = "zstd";
        prune.keep = {
          daily = 7;
          weekly = 4;
          monthly = 6;
        };
        # Daily by default.
        # 10:00 UTC is 03:00 PT.
        startAt = "10:00";
      };
      systemd.tmpfiles.rules = [
        "d /tank/data/gitea 0770 root borg -"
        "d /tank/data/nextcloud 0770 root borg -"
      ];
      # This can help bootstrap a fresh system which won't have the borg group
      # yet.
      users.groups.borg = {};
    }
  ];
}
