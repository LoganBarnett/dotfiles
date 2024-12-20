################################################################################
# Trivia: Selenium is used primarily for glass production and also makes for a
# good lead and sulfur replacement in many applications.  It is required for
# many forms of life in very trace amounts and very small amounts can lead to
# toxicity.
#
# Selenium provides an OctoPrint server for the Prusia 3D FFF printer.
################################################################################
{ disko-proper, flake-inputs }: let
  host-id = "selenium";
  system = "aarch64-linux";
in {
  imports = [
    (import ../nixos-modules/raspberry-pi-host.nix {
      inherit flake-inputs host-id;
    })
    {
      disabledModules = [
        "${flake-inputs.nixpkgs}/nixos/modules/services/misc/octoprint.nix"
      ];
      imports = [
        ../nixos-modules/octo-print-nixpkgs.nix
      ];
    }
    {
      services.octoprint = {
        enable = true;
        extraConfig = {
          accessControl = {
            userManager = "octoprint.access.users.FilebasedUserManager";
            autologinLocal = false;
          };
        };
        openFirewall = false;
        # Unfortunately plugins do not appear in the NixOS search page.  But you
        # can find them here:
        # https://github.com/NixOS/nixpkgs/blob/master/pkgs/by-name/oc/octoprint/plugins.nix
        plugins = pg: [
          pg.prusaslicerthumbnails
          pg.stlviewer
          pg.themeify
        ];
        # users = [];
      };
    }
    (import ../nixos-modules/https.nix {
      inherit host-id;
      fqdn = "${host-id}.proton";
      server-port = 5000;
    })
    (import ../nixos-modules/raspberry-pi-5.nix {
      inherit flake-inputs;
    })
    ../nixos-modules/nix-builder-provide.nix
    (import ../nixos-modules/server-host.nix {
      inherit flake-inputs host-id system;
    })
    # ../nixos-modules/raspberry-pi-disk.nix
    {
      # networking.hostId is needed by the filesystem stuffs.
      # An arbitrary ID needed for zfs so a pool isn't accidentally imported on
      # a wrong machine (I'm not even sure what that means).  See
      # https://search.nixos.org/options?channel=24.05&show=networking.hostId&from=0&size=50&sort=relevance&type=packages&query=networking.hostId
      # for docs.
      # Get from an existing machine using:
      # head -c 8 /etc/machine-id
      # Generate for a new machine using:
      # head -c4 /dev/urandom | od -A none -t x4
      networking.hostId = "16474474";
      nixpkgs.hostPlatform = system;
    }
  ];
}
