################################################################################
# Trivia: Selenium is used primarily for glass production and also makes for a
# good lead and sulfur replacement in many applications.  It is required for
# many forms of life in very trace amounts and very small amounts can lead to
# toxicity.
#
# Selenium provides an OctoPrint server for the Prusia 3D FFF/FDM printer.
#
# Sometimes the UI will be stuck in a redirect loop.  I'm not sure what breaks
# it out.  If the STL viewer is installed, it will come up first just before the
# redirect happens, but if the STL viewer is not installed, the redirect loop
# still occurs, so I think that is a false path.  I suspect it is involved and
# somehow bucks against authentication.
#
# Managing users here is a pain.  First, there must be an admin user in order
# for this to start.  No, you can't null out the password, but you can make the
# user inactive.  To bootstrap an admin user, you can use:
# sudo -u octoprint octoprint --basedir /var/lib/octoprint user add admin --admin
# You will likely have to grab the current octoprint bin path, using:
# systemctl cat octoprint.service
# Alternatively, you can just plop this into `/var/lib/octoprint/users.yaml`:
# admin:
#    active: false
#    apikey: null
#    groups:
#    - admins
#    password: $argon2id$v=19$m=65536,t=3,p=4$000000000000000000000000000000000000000000000000000000000000000000
#    permissions: []
#    roles:
#    - user
#    - admin
#    settings: {}
# Once you have that, you kind of have LDAP managed users but not really.  The
# documentation suggests you can setup a kind of group mapping that might make
# this work, but I don't have a working example and haven't experimented with it
# yet.  A user with read permission is not sufficient to log in, for some
# reason.  You'll need to add those permissions to the user by hand _after_
# attempting to log in as them - The LDAP plugin will write the user out to that
# same users.yaml file.  Once it's there, you can add `admins` to the `groups`
# list, and `admin` to the `roles` list.  From there you _must_ restart
# Octoprint for the changes to get picked up.
#
# Failure to create the admin user will result in weird refresh loop stuff when
# the browser hits the page.  Yay.  I think the typical workflow is that it
# redirects to the wizard and makes you configure things from there, but we
# turned that off.  During upgrades of Octoprint versions, you might find other
# oddities like that.
#
# During the time I last ran into this, I had `accessControl = false;` and that
# could be related, but I haven't done anything to confirm this.
################################################################################
{ config, flake-inputs, host-id, lib, pkgs, system, ... }: {
  imports = [
    ../nixos-modules/octoprint-shim.nix
    ../nixos-modules/raspberry-pi-5.nix
    ../nixos-modules/nix-builder-provide.nix
    ../nixos-modules/server-host.nix
    ../nixos-modules/facts-secrets.nix
    {
      age.secrets = config.lib.ldap.ldap-password
        "octoprint"
        "${host-id}-octoprint-service"
      ;
      # Used in conjunction with the libraspberrypi library (which provides the
      # vcgencmd command), we should be able to see under-voltage reports in
      # OctoPrint.
      users.users.octoprint.extraGroups = [
        "video"
        "openldap-${host-id}-octoprint-service"
      ];
      services.https.fqdns."selenium.proton" = {
        enable = true;
        internalPort = config.services.octoprint.port;
      };
      users.groups."openldap-${host-id}-octoprint-service" = {};
      services.nginx = {
        # Allow large uploads, because we actually do send large files.  This is
        # probably crazy but hey we won't have to adjust it again.
        clientMaxBodySize = "1g";
      };
      services.octoprint = {
        enable = true;
        defaultPrinterProfile = "prusa-xl";
        printerProfiles = {
          # If the origin is not set on the lower left for various axes,
          # OctoPrint will prevent the print, but clicking print again works.
          # Better to just fix it though.
          prusa-xl = {
            axes = {
              e = {
                inverted = false;
                speed = 300;
              };
              x = {
                inverted = false;
                origin = "lowerleft";
                speed = 6000;
              };
              y = {
                inverted = false;
                origin = "lowerleft";
                speed = 6000;
              };
              z = {
                inverted = false;
                origin = "lowerleft";
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
              origin = "lowerleft";
              width = 360.0;
            };
          };
        };
        extraConfig = {
          accessControl = {
            autologinLocal = false;
            userManager = "octoprint.access.users.LDAPUserManager";
          };
          # api = {
          #   enabled = false;
          #   allowCrossOrigin = true;
          # };
          commands = {
            # This should disable some of the errors spewed due to constant
            # checks.
            localPipCommand = "None";
          };
          plugins = {
            _disabled = [
              # Core / UI
              "achievements"
              "announcements"
              "corewizard"
              # "customcontrolmanager"
              # "eventmanager"
              # "logging"
              # "uploadmanager"
              # Auth / keys / security-adjacent
              # "appkeys"
              # "tracking"
              # "errortracking"
              # "health_check"
              # Printer / state / UI bindings
              # "action_command_notification"
              # "action_command_prompt"
              # "gcodeviewer"
              # "virtual_printer"
              # Discovery / network
              # "discovery"
              # Webcam
              # "classicwebcam"
              # Maintenance / updates
              # "backup"
              "pluginmanager"
              "softwareupdate"
              # Platform-specific
              "pi_support"
              # Validation helpers
              # "file_check"
              # "firmware_check"
            ];
            # Per https://github.com/gillg/OctoPrint-LDAP/issues/5 the log level
            # for all of Octoprint can be set to `DEBUG` and we should see more
            # errors.  Right now I see nothing.
            auth_ldap = {
              uri = "ldaps://nickel.proton";
              auth_user = "uid=${host-id}-octoprint-service,ou=users,dc=proton,dc=org";
              # TODO: Cycle this out once this is securely referenced.
              # This is supported via my open (as of [2025-02-22]) pull request:
              # https://github.com/gillg/OctoPrint-LDAP/pull/28
              auth_password_file = config
                .age
                .secrets
                ."${host-id}-octoprint-service-ldap-password"
                .path
              ;
              search_base = "dc=proton,dc=org";
              # (&(ou_filter)(ou_member_filter))
              # (&(cn=%s,ou=groups,dc=proton,dc=org)(member=uid=%s,ou=users,dc=proton,dc=org))
              # (&(cn=3d-printers,ou=groups,dc=proton,dc=org)(member=uid=logan,ou=users,dc=proton,dc=org))
              ou_filter = "cn=%s";
              ou_member_filter = "member=%s";
              ou = "3d-printer-printers, 3d-printer-admins";
              default_admin_group = true;
              # There's some group mapping stuff I need to work out, but it only
              # works assuming a single prefix mapping.  It is mentioned in the
              # documentation, but not actually documented.
              # ldap_group_key_prefix = "3d-printer-";
              # ldap_parent_group_description = ''
              #   Generated by Auth LDAP plugin, with membership synced \
              #   automatically based on LDAP configuration.
              # '';
              # ldap_parent_group_key = "ldap";
              # ldap_parent_group_name = "";
            };
          };
          serial = {
            autoconnect = true;
          };
          server = {
            # Prevent us from entering the wizard which won't work because the
            # settings are fixed... at least they should be.
            firstRun = false;
            seenWizards = {
              backup = "null";
              classicwebcam = 1;
              corewizard = 4;
              tracking = "null";
            };
          };
        };
        openFirewall = false;
        # Unfortunately plugins do not appear in the NixOS search page.  But you
        # can find them here:
        # https://github.com/NixOS/nixpkgs/blob/master/pkgs/by-name/oc/octoprint/plugins.nix
        plugins = pg: [
          # Show progress on the printer's display.
          pg.displayprogress
          # Show progress via the M117 command - better somehow?
          pg.displaylayerprogress
          # View slicer thumbnails in the file list.
          pg.prusaslicerthumbnails
          # Allow viewing the (equivalent?) STL while printing.
          pg.stlviewer
          # Save our eyes by letting us load a dark mode theme.
          pg.themeify
          # bgcode is Prusa's binary format.  It's not strictly needed and the
          # space savings aren't impactful in a meaningful way for us.  I'm not
          # sure if this plugin is even supported.  However that does mean that
          # a Prusa Slicer configuration needs to be set to use gcode instead of
          # bgcode.
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
          # A needed PR I submitted has been merged, but there is no release for
          # it that I know of yet.
          (pg.buildPlugin (let
            version = "2022-11-10-unstable";
          in {
            pname = "octoprint-plugin-authldap";
            inherit version;
            format = "setuptools";
            src = pkgs.fetchFromGitHub {
              owner = "gillg";
              repo = "OctoPrint-LDAP";
              rev = "841504487d7c5e634a93e4bd32c71f06b974ed36";
              hash = "sha256-xxwkOgQUUt5JmqjFZIXPipH6zvq8ysTbOaU3h6RLNzs=";
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
        # This doesn't work on Raspberry Pi 5 and up.
        raspberryPiVoltageThrottlingCheck = false;
        # users = [];
      };
    }
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
