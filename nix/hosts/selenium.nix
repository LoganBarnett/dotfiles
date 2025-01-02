################################################################################
# Trivia: Selenium is used primarily for glass production and also makes for a
# good lead and sulfur replacement in many applications.  It is required for
# many forms of life in very trace amounts and very small amounts can lead to
# toxicity.
#
# Selenium provides an OctoPrint server for the Prusia 3D FFF printer.
################################################################################
{ disko-proper, flake-inputs }: { pkgs, ... }: let
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
            userManager = "octoprint.access.users.LDAPUserManager";
            autologinLocal = false;
          };
          plugins = {
            auth_ldap = {
              uri = "ldaps://nickel.proton";
              auth_user = "cn=selenium-octoprint-service,ou=users,dc=proton,dc=org";
              auth_password = "no one would use one two three for five four their password";
              search_base = "dc=proton,dc=org";
              ou_filter = "cn=%s";
              ou = "3d-printers";
            };
          };
          server = {
            # Prevent us from entering the wizard which won't work because the
            # settings are fixed... at least they should be.
            firstRun = false;
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
          (pg.buildPlugin (let
            version = "2024-05-29-unstable";
          in {
            pname = "authldap";
            inherit version;
            src = pkgs.fetchFromGitHub {
              owner = "jneilliii";
              repo = "OctoPrint-BGCode";
              rev = "2e01626731213f362e450f3b175b64d5a1c434c2";
              hash = "sha256-J2jvNJJMrD5uRpSDxO+oujfphELwNlYoTFS5VvMBuB8=";
            };
            propagatedBuildInputs = [];
            meta = {
              description = "Bring BGCode support to OctoPrint.";
              homepage = "https://github.com/jneilliii/OctoPrint-BGCode";
              # maintainers = with lib.maintainers; [ logan-barnett ];
            };
          }))
          (pg.buildPlugin (let
            version = "2022-11-10-unstable";
          in {
            pname = "authldap";
            inherit version;
            src = pkgs.fetchFromGitHub {
              owner = "gillg";
              repo = "OctoPrint-LDAP";
              rev = "473cf955309b8ba427d4c6b5f50b4d7b58c56477";
              hash = "sha256-6b5IXCIOxLlyKLo17y9gh30gQF7S0OEJVhTF6U2hrz0=";
            };
            propagatedBuildInputs = [
              pkgs.python3Packages.python-ldap
            ];
            meta = {
              description = "Bring LDAP authentication to OctoPrint.";
              homepage = "https://github.com/gillg/OctoPrint-LDAP";
              # maintainers = with lib.maintainers; [ logan-barnett ];
            };
          }))
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
