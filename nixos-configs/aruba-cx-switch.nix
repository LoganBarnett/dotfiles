################################################################################
# nix-hapi tree for managing the Aruba CX 6300M switch.
#
# Declares VLANs and port assignments through the typed
# nix-hapi-aruba-cx module.  The reconciler keeps the switch's
# desired state in sync against its REST API.
################################################################################
{
  config,
  flake-inputs,
  system,
  ...
}:
{
  imports = [
    flake-inputs.nix-hapi-provider-aruba-cx.nixosModules.default
  ];

  services.nix-hapi-aruba-cx = {
    enable = true;
    package = flake-inputs.nix-hapi-provider-aruba-cx.packages.${system}.default;

    scopes."core-switch" = {
      provider = {
        base_url = "https://192.168.254.200";
        username = "admin";
        password = {
          __nixhapi = "managed-from-path";
          path = config.age.secrets.aruba-cx-password.path;
        };
      };

      vlans = {
        "10" = {
          name = "Main";
          description = "Primary LAN — servers, workstations";
        };
        "20" = {
          name = "IoT";
          description = "IoT devices — isolated by default";
        };
        "30" = {
          name = "Guest";
          description = "Guest network — internet only";
        };
        "100" = {
          name = "WAN";
          description = "ISP uplink from ONT";
        };
      };

      ports = {
        # Port 1: trunk to silicon (all VLANs tagged).
        "1/1/1" = {
          vlan_mode = "trunk";
          vlan_trunks = [
            10
            20
            30
            100
          ];
          description = "Trunk to silicon";
        };
        # Port 2: ONT / ISP uplink (access VLAN 100).
        "1/1/2" = {
          vlan_mode = "access";
          vlan_tag = 100;
          description = "ONT / ISP uplink";
        };
        # Ports 3+: default to Main VLAN.  Reassign individual ports to
        # IoT (20) or Guest (30) as devices are migrated.
        "1/1/3" = {
          vlan_mode = "access";
          vlan_tag = 10;
          description = "Main LAN";
        };
        "1/1/4" = {
          vlan_mode = "access";
          vlan_tag = 10;
          description = "Main LAN";
        };
        "1/1/5" = {
          vlan_mode = "access";
          vlan_tag = 10;
          description = "Main LAN";
        };
        "1/1/6" = {
          vlan_mode = "access";
          vlan_tag = 10;
          description = "Main LAN";
        };
      };

      # Ignore VLAN 1 (default) — always present on the switch.
      ignore = [
        ''.key | startswith("1/")''
      ];
    };
  };

  # agenix secret for the switch admin password.
  age.secrets.aruba-cx-password = {
    rekeyFile = ../secrets/aruba-cx-password.age;
  };
}
