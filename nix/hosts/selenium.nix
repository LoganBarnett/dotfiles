################################################################################
# Trivia: Selenium is used primarily for glass production and also makes for a
# good lead and sulfur replacement in many applications.  It is required for
# many forms of life in very trace amounts and very small amounts can lead to
# toxicity.
#
# Selenium provides an OctoPrint server for the Prusia 3D FFF printer.
################################################################################
{ config, flake-inputs, pkgs, ... }: let
  host-id = "selenium";
  system = "aarch64-linux";
in {
  imports = [
    ../nixos-modules/octoprint-shim.nix
    ../nixos-modules/raspberry-pi-5.nix
    ../nixos-modules/nix-builder-provide.nix
    ../nixos-modules/server-host.nix
    ../nixos-modules/facts-secrets.nix
    {
      age.secrets = config.lib.ldap.ldap-password
        "octoprint"
        "selenium-octoprint-service"
      ;
      services.nginx = {
        clientMaxBodySize = "1g";
      };
      services.octoprint = {
        enable = true;
        printerProfiles = {
          _default = {
            axes = {
              e = {
                inverted = false;
                speed = 300;
              };
              x = {
                inverted = false;
                speed = 6000;
              };
              y = {
                inverted = false;
                speed = 6000;
              };
              z = {
                inverted = false;
                speed = 200;
              };
            };
            color = "default";
            extruder = {
              count = 2;
              defaultExtrusionLength = 5;
              nozzleDiameter = 0.4;
              offsets = [
                [ 0.0 0.0 ]
                [ 0.0 0.0 ]
              ];
              sharedNozzle = false;
            };
            heatedBed = true;
            heatedChamber = false;
            model = "Prusa XL";
            name = "prusa-xl";
            volume = {
              custom_box = false;
              depth = 360.0;
              formFactor = "rectangular";
              height = 360.0;
              origin = "lowerLeft";
              width = 360.0;
            };
          };
        };
        extraConfig = {
          accessControl = {
            userManager = "octoprint.access.users.LDAPUserManager";
            autologinLocal = false;
          };
          plugins = {
            # Per https://github.com/gillg/OctoPrint-LDAP/issues/5 the log level
            # for all of Octoprint can be set to `DEBUG` and we should see more
            # errors.  Right now I see nothing.
            auth_ldap = {
              uri = "ldaps://nickel.proton";
              auth_user = "uid=selenium-octoprint-service,ou=users,dc=proton,dc=org";
              # TODO: Cycle this out once this is securely referenced.
              # This is supported via my open (as of [2025-02-22]) pull request:
              # https://github.com/gillg/OctoPrint-LDAP/pull/28
              auth_password_file = config
                .age
                .secrets
                .selenium-octoprint-service-ldap-password
                .path
              ;
              search_base = "dc=proton,dc=org";
              # (&(ou_filter)(ou_member_filter))
              # (&(cn=%s,ou=groups,dc=proton,dc=org)(member=uid=%s,ou=users,dc=proton,dc=org))
              # (&(cn=3d-printers,ou=groups,dc=proton,dc=org)(member=uid=logan,ou=users,dc=proton,dc=org))
              ou_filter = "cn=%s";
              ou_member_filter = "member=%s";
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
          # pg.prusaslicerthumbnails
          # pg.stlviewer
          # pg.themeify
          # (pg.buildPlugin (let
          #   version = "2024-05-29-unstable";
          # in {
          #   pname = "octoprint-plugin-bgcode";
          #   inherit version;
          #   src = pkgs.fetchFromGitHub {
          #     owner = "jneilliii";
          #     repo = "OctoPrint-BGCode";
          #     rev = "2e01626731213f362e450f3b175b64d5a1c434c2";
          #     hash = "sha256-J2jvNJJMrD5uRpSDxO+oujfphELwNlYoTFS5VvMBuB8=";
          #   };
          #   propagatedBuildInputs = [
          #     pkgs.libbgcode
          #   ];
          #   meta = {
          #     description = "Bring BGCode support to OctoPrint.";
          #     homepage = "https://github.com/jneilliii/OctoPrint-BGCode";
          #     # maintainers = with lib.maintainers; [ logan-barnett ];
          #   };
          # }))
          (pg.buildPlugin (let
            version = "2022-11-10-unstable";
          in {
            pname = "octoprint-plugin-authldap";
            inherit version;
            src = pkgs.fetchFromGitHub {
              owner = "gillg";
              repo = "OctoPrint-LDAP";
              rev = "473cf955309b8ba427d4c6b5f50b4d7b58c56477";
              hash = "sha256-6b5IXCIOxLlyKLo17y9gh30gQF7S0OEJVhTF6U2hrz0=";
            };
            patches = [
              # ../nixos-modules/octoprint-auth-ldap-extra-logging.patch
              ../nixos-modules/octoprint-auth-ldap-password-file.patch
            ];
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
