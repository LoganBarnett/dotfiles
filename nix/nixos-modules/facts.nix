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
    # The nfsVolumes here have the following structure:
    # {
    #   host-id = "oganesson";
    #   peerNumber = 118;
    #   volume = "photos";
    # };
    #
    # - The hostname is the hostname portion of the host's FQDN.
    # - peerNumber is used to build the host's IP used in the WireGuard network
    #   used to secure the NFS shares.
    # - volume is the name of the volume to share, which will expand to
    #   "/tank/data/${volume}".
    nfsVolumes = [
      # Currently, nextcloud "works", so don't mess with it until we have solid
      # verification on a system that is not yet critical or known to be
      # delicately functional.
      {
        consumerHostId = "copper";
        providerHostId = "silicon";
        peerNumber = 3;
        service = "nextcloud";
        volume = "nextcloud";
        user = "nextcloud";
        group = "nextcloud";
        gid = 29971;
      }
      {
        consumerHostId = "copper";
        providerHostId = "silicon";
        peerNumber = 3;
        service = "gitea";
        volume = "gitea";
        user = "gitea";
        group = "gitea";
        gid = 29972;
      }
    ];
    ##
    # The hosts here have the following structure:
    # "${hostname}" = {
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
    #
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
        flake-input-overrides = {
          nixpkgs = "nixpkgs-nixos-raspberrypi";
        };
        ipv4 = 2;
        monitors = [
          "node"
          "systemd"
        ];
        networkInterface = "enu1u1";
        system = "aarch64-linux";
      };
      arsenic = {
        aliases = [
          "ollama"
        ];
        controlledHost = true;
        flake-input-overrides = {
          nixpkgs = "nixpkgs-cuda";
        };
        ipv4 = 7;
        monitors = [
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
          "mosquitto"
          "openhab"
          "zwave-js-ui"
        ];
        controlledHost = true;
        ipv4 = 3;
        monitors = [
          "node"
          "systemd"
        ];
        system = "aarch64-linux";
      };
      cobalt = {
        aliases = [
          "rpi-build"
        ];
        controlledHost = true;
        ipv4 = 11;
        monitors = [
          "node"
          "systemd"
        ];
        system = "aarch64-linux";
      };
      copper = {
        aliases = [
          "chronicle-proxy"
          "dex"
          "gitea"
          "nextcloud"
        ];
        controlledHost = true;
        ipv4 = 10;
        monitors = [
          "node"
          "systemd"
        ];
        system = "x86_64-linux";
      };
      gallium = {
        aliases = [
          "authelia"
          "jenkins"
          "ldap"
          "sso"
        ];
        controlledHost = true;
        ipv4 = 4;
        monitors = [
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
      lithium = {
        controlledHost = true;
        flake-input-overrides = {
          nixpkgs = "nixpkgs-cuda";
        };
        ipv4 = 8;
        monitors = [
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
          nixpkgs = "nixpkgs-nixos-raspberrypi";
        };
        ipv4 = 1;
        monitors = [
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
      rpi-installer = {
        controlledHost = false;
        ipv4 = 253;
        monitors = [];
        system = "aarch64-linux";
      };
      # A WiFi endpoint / extender / repeater, depending on how it is
      # configured.  I have it in the endpoint/extender mode, but its
      # configuration can go astray if it doesn't wake up in a positive state.
      # This is the default hostname.  I'm not sure how to configure it, and I
      # have two of them.  I'm unsure how to address them further.  But this
      # device is configured to have a static IP which is the same that I have
      # here.
      re500x = {
        controlledHost = false;
        # Not necessarily the most compact.  I was troubleshooting and needed to
        # pick something I was fairly certain was available.  Find lower IPs if
        # you're adding a new host.
        ipv4 = 20;
        monitors = [];
      };
      scandium = {
        controlledHost = true;
        flake-input-overrides = {
          nixpkgs = "nixpkgs";
        };
        monitors = [];
        roaming = true;
        system = "aarch64-darwin";
      };
      selenium = {
        controlledHost = true;
        flake-input-overrides = {
          nixpkgs = "nixpkgs-nixos-raspberrypi";
        };
        ipv4 = 5;
        monitors = [
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
        macAddress = "b8:ca:3a:77:a9:2a";
        monitors = [
          "node"
          "systemd"
          "wireguard"
        ];
        networkInterface = "eno1";
        system = "x86_64-linux";
      };
      titanium = {
        aliases = [
          "musicgpt"
        ];
        controlledHost = true;
        ipv4 = 6;
        monitors = [
          "node"
          "systemd"
        ];
        system = "x86_64-linux";
      };

    }
      # This is sort of miscellaneous hosts that aren't part of critical
      # infrastructure.  I want them tracked so I know what kind of block
      # profiles they are, and I have a handy lookup should I need to identify
      # them later.
      # "uih" is short for unidentified host.
    // {
      uih0 = { mac = "ac:19:8e:f6:04:a0"; ipv4 = 120; };
      some-unidentified-iphone = { mac = "ea:c6:99:b2:df:de"; ipv4 = 121; };
      uih1 = { mac = "f0:a2:25:49:25:c8"; ipv4 = 122; };
      uih2 = { mac = "32:ca:84:c2:82:7b"; ipv4 = 123; };
      uih3 = { mac = "66:f2:6e:b8:a5:16"; ipv4 = 124; };
      uih4 = { mac = "10:2c:b1:a5:ad:68"; ipv4 = 125; };
      uih5 = { mac = "10:2c:b1:76:68:5d"; ipv4 = 126; };
      uih6 = { mac = "ac:19:8e:1a:81:19"; ipv4 = 127; };
      uih7 = { mac = "92:0a:15:a6:00:07"; ipv4 = 128; };
      uih8 = { mac = "c8:a3:62:84:33:99"; ipv4 = 129; };
      uih9 = { mac = "5e:8c:33:66:1d:65"; ipv4 = 130; };
      uih10 = { mac = "76:3c:6c:a6:57:3c"; ipv4 = 131; };
      uih11 = { mac = "28:cf:51:c9:1d:01"; ipv4 = 132; };
      uih12 = { mac = "92:8b:79:8a:9e:c4"; ipv4 = 133; };
      uih13 = { mac = "80:f3:ef:3b:b6:4d"; ipv4 = 134; };
      uih14 = { mac = "10:2c:b1:8e:0f:ee"; ipv4 = 135; };
      uih15 = { mac = "06:8a:dc:63:fe:e6"; ipv4 = 136; };
      Tonal-080300100010266 = { mac = "10:59:17:00:cf:4f"; ipv4 = 137; };
      Canonc618b2 = { mac = "20:0b:74:9a:21:2b"; ipv4 = 138; };
      SOUNDPLUS_X_E0A8 = { mac = "02:22:6c:07:e0:a8"; ipv4 = 139; };
      tasmota-8CFDB2-7602 = { mac = "d8:bc:38:8c:fd:b2"; ipv4 = 140; };
      # What is this?
      ESP_45248A = { mac = "cc:7b:5c:45:24:8a"; ipv4 = 141; };
      # What is this?
      S380HB = { mac = "90:bf:d9:3e:20:d8"; ipv4 = 142; };
      WYZEC1-JZ-2CAA8E995847 = { mac = "2c:aa:8e:99:58:47"; ipv4 = 143; };
      some-unidentified-apple-watch = { mac = "ee:b0:c4:e8:3e:c6"; ipv4 = 144; };
      some-unidentified-mac-book-pro = { mac = "3e:8f:45:9d:b3:cd"; ipv4 = 145; };
      iRobot-E49F9CCC561A410AB51B88723BB7BB1E = { mac = "50:14:79:b7:a1:b5"; ipv4 = 146; };
      amazon-1fa3b1b81 = { mac = "40:b4:cd:35:85:61"; ipv4 = 147; };
      roku3-667 = {
        controlledHost = false;
        # TODO: Find a better IP.
        ipv4 = 148;
        mac = "b0:a7:37:96:c6:33";
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
        cassandra = {
          description = "";
          email = "cassandra@proton";
          type = "person";
          full-name = "Cassandra Barnett";
          devices = [];
          blockProfiles = [
            "adult"
          ];
        };
        kai = {
          description = "";
          email = "kai@proton";
          type = "person";
          full-name = "Kai Barnett";
          devices = [
            { host-id = "arsenic"; vpn = false; }
          ];
          blockProfiles = [
            "child"
          ];
        };
        logan = {
          description = "The reason we suffer.";
          email = "logustus@proton";
          type = "person";
          full-name = "Logan Barnett";
          devices = [
            { host-id = "scandium"; ip = "20"; vpn = true; }
            { host-id = "manganese"; ip = "22"; vpn = true; }
          ];
          blockProfiles = [
            "adult"
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
          blockProfiles = [
            "adult"
          ];
        };
        solomon = {
          description = "";
          email = "solomon@proton";
          type = "person";
          full-name = "Solomon Barnett";
          devices = [
            { host-id = "lithium"; vpn = false; }
          ];
          blockProfiles = [
            "child"
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
        # bromine-authelia-home-assistant-service = {
        #   email = "bromine-authelia-home-assistant-service@proton";
        #   type = "service";
        #   description = "Authelia authentication for Home Assistant on Bromine.";
        #   full-name = "bromine-authelia-home-assistant-service";
        #   devices = [];
        # };
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
        nickel-authelia-service = {
          email = "nickel-authelia-service@proton";
          type = "service";
          description = "Authelia on Nickel.  For SSO authentication via OIDC.";
          full-name = "nickel-authelia-service";
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
      } // {
        openhab-oidc-client = {
          email = "openhab-oidc-client@proton";
          type = "oidc-client";
          description = "OpenHab OIDC client.";
          full-name = "openhab-oidc-client";
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
      "gitea-admins" = {
        description = "People who can administer Gitea.";
        members = [
          "logan"
        ];
      };
      "gitea-users" = {
        description = "People who can user Gitea.";
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
      "home-assistant-admins" = {
        description = "People who can administer Home Assistant.";
        members = [
          "logan"
        ];
      };
      "home-assistant-users" = {
        description = "People who can use Home Assistant.";
        members = [
          "cassandra"
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
      openhab-users = {
        description = "People who can use OpenHab.";
        members = [
          "logan"
          "cassandra"
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

    services = {

      openhab = {
        authentication = "oidc";
        fqdn = "openhab.proton";
        groups = [
          "openhab-admins"
          "openhab-users"
        ];
        # redirectUris = [
        #   "https://openhab.proton/outpost.goauthentik.io/callback"
        # ];
      };

      nextcloud = {
        authentication = "ldap";
        fqdn = "nextcloud.proton";
        groups = [
          "nextcloud-admins"
          "nextcloud-users"
        ];
        healthCheck = {
          subUri = "/status.php";
          conditions = [
            "[STATUS] == 200"
            "[BODY].installed == true"
            "[BODY].maintenance == false"
            "[RESPONSE_TIME] < 300"
            # Must have >7 days left.
            "[CERTIFICATE_EXPIRATION] > 168h"
          ];
        };
      };

    };

  };
}
