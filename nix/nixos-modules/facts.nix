################################################################################
# I've other people use the nomenclature of "facts", and it kind of makes sense
# because "settings" would be a very ambiguous term.  "Facts" is only de facto
# via Puppet, as I understand.
#
# It means settings, but more like a meta settings.  Settings here should be
# logical instead of pertaining to specific systems whenever possible.
################################################################################
{
  network = {
    # I have a naming theme of periodic elements and particles.
    domain = "proton";
    # These are just stringly prefixes for IP addresses.  At some point I should
    # turn these into real subnets with masks.
    subnets = {
      # The main network, whose atypical subnet is a vestigial dictation by the
      # consumer router.
      barnett-main = "192.168.254";
    };
    ##
    # The hosts here have the following structure:
    # ${hostname} = {
    #  controlledHost = boolean;
    #  flake-input-overrides = {
    #    nixpkgs = "nixpkgs-working-rocm";
    #  };
    #  ipv4 = 123;
    #  monitors = [
    #    "node"
    #    "systemd"
    #  ];
    #  roaming = boolean;
    #  system = "aarch64-linux";
    # };
    # - The hostname is the hostname portion of the host's FQDN.
    # - controlledHost indicates this is a host that is controlled completely by
    #   our configuration here.  This can have many implications.  A
    #   non-controlled host being monitored will be the target of a ping, for
    #   example.
    # - flake-input-overrides (optional) allows remapping one of the flake
    #   inputs to override another.  A common case for this is to override
    #   nixpkgs, but could be used for any combination of inputs.  The key is
    #   the name if the input to override, and the value is a String that
    #   matches the key from flake-inputs to substitute.
    # - ipv4 The host portion of this host's IP address.  This will be fixed in
    #   the DNS.  Leaving this out means the host uses DHCP.  Alternatively it
    #   means we're talking about a container.
    # - monitors A list of monitors to use for this host.
    # - roaming (optional, default false) Whether or not this host roams on and
    #   off this network.  This is generally intended for personal laptops that
    #   will frequently be offline, and thus should not trigger alerts when
    #   offline.
    # - system The architecture-platform "double" suitable for the hostPlatform
    #   setting.
    # See https://stackoverflow.com/a/70106730 for how to setup Prometheus for
    # monitoring remote / uncontrolled systems (remote_read vs. remote_write).
    # There's a lot of silly work I do to fix this stuff.  I should write some
    # validators for this.
    hosts = {
      argon = {
        controlledHost = true;
        ipv4 = 2;
        monitors = [
          "blackbox-ping"
          "node"
          "systemd"
          "wireguard"
        ];
        system = "aarch64-linux";
      };
      arsenic = {
        controlledHost = true;
        flake-input-overrides = {
          nixpkgs = "nixpkgs-cuda";
        };
        ipv4 = 7;
        monitors = [
          "blackbox-ping"
          "node"
          "nvidia-gpu"
          "systemd"
        ];
        system = "x86_64-linux";
      };
      bromine = {
        aliases = [
          "matrix"
          "home-assistant"
        ];
        controlledHost = true;
        ipv4 = 3;
        monitors = [
          "blackbox-ping"
          "node"
          "systemd"
        ];
        system = "aarch64-linux";
      };
      copper = {
        aliases = [
          "dex"
          "nextcloud"
        ];
        controlledHost = true;
        flake-input-overrides = {
          nixpkgs = "nixpkgs-25";
        };
        ipv4 = 10;
        monitors = [
          "blackbox-ping"
          "node"
          "systemd"
        ];
        system = "x86_64-linux";
      };
      gallium = {
        aliases = [];
        controlledHost = true;
        flake-input-overrides = {
          nixpkgs = "nixpkgs-25";
        };
        ipv4 = 4;
        monitors = [
          "blackbox-ping"
          "node"
          "systemd"
        ];
        system = "aarch64-linux";
      };
      # germanium = {
      #   controlledHost = true;
      #   monitors = [
      #     "node"
      #     "systemd"
      #   ];
      #   system = "x86_64-linux";
      # };
      gateway = {
        controlledHost = false;
        ipv4 = 254;
        monitors = [];
      };
      grafana = {
        controlledHost = true;
        monitors = [];
      };
      lithium = {
        controlledHost = true;
        flake-input-overrides = {
          nixpkgs = "nixpkgs-cuda";
        };
        ipv4 = 8;
        monitors = [
          "blackbox-ping"
          "node"
          "nvidia-gpu"
          "systemd"
        ];
        system = "x86_64-linux";
      };
      "M-CL64PK702X" = {
        controlledHost = true;
        flake-input-overrides = {
          nixpkgs = "nixpkgs-latest";
        };
        monitors = [];
        roaming = true;
        system = "aarch64-darwin";
      };
      nickel = {
        aliases = [
          "alertmanager"
          "grafana"
          "prometheus"
        ];
        controlledHost = true;
        flake-input-overrides = {
          # Give Prometheus the ability to see the nvidia-gpu exporter.
          nixpkgs = "nixpkgs-cuda";
        };
        ipv4 = 1;
        monitors = [
          "blackbox-ping"
          # "openldap"
          "node"
          "systemd"
        ];
        system = "aarch64-linux";
      };
      nucleus = {
        controlledHost = false;
        monitors = [];
        system = "x86_64-linux";
      };
      scandium = {
        controlledHost = true;
        flake-input-overrides = {
          nixpkgs = "nixpkgs-latest";
        };
        monitors = [];
        roaming = true;
        system = "aarch64-darwin";
      };
      selenium = {
        controlledHost = true;
        flake-input-overrides = {
          nixpkgs = "nixpkgs-rpi";
        };
        ipv4 = 5;
        monitors = [
          "blackbox-ping"
          # "octoprint"
          "node"
          "systemd"
        ];
        system = "aarch64-linux";
      };
      silicon = {
        controlledHost = true;
        flake-input-overrides = {};
        ipv4 = 9;
        monitors = [
          "blackbox-ping"
          "node"
          "systemd"
        ];
        system = "x86_64-linux";
      };
      titanium = {
        controlledHost = true;
        flake-input-overrides = {
          home-manager = "home-manager-working-rocm";
          nixpkgs = "nixpkgs-working-rocm";
        };
        ipv4 = 6;
        monitors = [
          "blackbox-ping"
          "node"
          "systemd"
        ];
        system = "x86_64-linux";
      };
    };
    ##
    # A user has the following structure:
    # ${username} = {
    #  description = "Description of user.";
    #  email = "email@domain";
    #  type = "person";
    #  full-name = "First Last";
    #  devices = [
    #    { host-id = "hostname"; ip = "20"; vpn = true; }
    #  ];
    # };
    users =
      # Interactive / human users.
      {
        logan = {
          description = "The reason we suffer.";
          email = "logustus@proton";
          type = "person";
          full-name = "Logan Barnett";
          devices = [
            { host-id = "scandium"; ip = "20"; vpn = true; }
            { host-id = "manganese"; ip = "22"; vpn = true; }
          ];
        };
        selena = {
          description = "";
          email = "selena@proton";
          type = "person";
          full-name = "Selena";
          devices = [
            { host-id = "selena-laptop"; ip = "23"; vpn = true; }
          ];
        };
      }
      # Service / non-interactive users.
      # TODO: I need a better way of declaring these here and in the modules
      # where they are used.  If I forget to set up the service account here, it
      # simply won't get used in the LDAP server, and things just fail without
      # much indication that I'm missing a declaration here.  Perhaps I need a
      # service account function that validates the presence here, or gets the
      # values it needs from here. That way I have a nice error trail that leads
      # me to the correct conclusion quickly.
      // {
        bromine-matrix-service = {
          email = "bromine-matrix-service@proton";
          type = "service";
          description = "Matrix on Bromine.";
          full-name = "bromine-matrix-service";
          devices = [];
        };
        copper-dex-oidc-service = {
          email = "bromine-dex-oidc-service@proton";
          type = "service";
          description = "Dex-OIDC on Copper.";
          full-name = "copper-dex-oidc-service";
          devices = [];
        };
        copper-nextcloud-service = {
          email = "copper-nextcloud-service@proton";
          type = "service";
          description = "Nextcloud on Copper.";
          full-name = "copper-nextcloud-service";
          devices = [];
        };
        nickel-alertmanager-service = {
          email = "nickel-alertmanager-service@proton";
          type = "service";
          description = "AlertManager on Nickel.  Primarily for posting alerts.";
          full-name = "nickel-alertmanager-service";
          devices = [];
        };
        nickel-grafana-service = {
          email = "nickel-grafana-service@proton";
          type = "service";
          description = "Grafana on Nickel.";
          full-name = "nickel-grafana-service";
          devices = [];
        };
        selenium-octoprint-service = {
          email = "selenium-octoprint-service@proton";
          type = "service";
          description = "Octoprint on Selenium.";
          full-name = "selenium-octoprint-service";
          devices = [];
        };
      };
    groups = {
      "3d-printer-admins" = {
        description = "People who can administer Octoprint.";
        members = [
          "logan"
        ];
      };
      "3d-printer-printers" = {
        description = "People who can use 3D printers.";
        members = [
          "logan"
        ];
      };
      "grafana-admins" = {
        description = "People who can administer Grafana.";
        members = [
          "logan"
        ];
      };
      "grafana-viewers" = {
        description = "People who can view dashboards in Grafana.";
        members = [
          "logan"
        ];
      };
      "matrix-admins" = {
        description = "People who can administer Matrix.";
        members = [
          "logan"
        ];
      };
      "matrix-users" = {
        description = "People who can use Matrix.";
        members = [
          "logan"
          "nickel-alertmanager-service"
        ];
      };
      "nextcloud-admins" = {
        description = "People who can administer Nextcloud.";
        members = [
          "logan"
        ];
      };
      "nextcloud-users" = {
        description = "People who can use Nextcloud.";
        members = [
          "logan"
        ];
      };
      screen-addicts = {
        description =
          "Users who are addicted to screens and require help with "
          + "self-regulation."
        ;
        members = [
          "kai"
          "solomon"
        ];
      };
    };
  };
}
