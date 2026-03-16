################################################################################
# Ensure this host has a TLS certificate available which is tied to the internal
# CA.
################################################################################
{ config, host-id, lib, pkgs, ... }: let
  cfg = config.tls;
  inherit (lib) mkEnableOption mkIf mkMerge mkOption types;
in {
  # TODO: Make this `security.tls`.
  options.tls = {
    enable = mkEnableOption "Manage TLS certificates." // { default = true; };
    tls-leafs = mkOption (let
      tls-leaf-type = types.submodule {
        options = {
          enable = mkEnableOption "Manage a TLS leaf certificate." // {
            default = true;
          };
          fqdn = mkOption {
            type = types.str;
          };
          ca = mkOption {
            # Not sure how to type this correctly.  Come back to it later.
            type = types.anything;
          };
        };
      };
    in {
      type = types.attrsOf tls-leaf-type;
      default = {};
    });
  };

  config = mkIf cfg.enable (let
    tls-leafs = lib.filter
      (leaf: leaf.enable)
      (lib.attrsets.attrValues cfg.tls-leafs)
    ;
  in {
    users.groups = {
      tls-leaf = {};
    };
    age.secrets = mkMerge (
      map
        (leaf-config: {
          "tls-${leaf-config.fqdn}.key" = {
            generator = {
              dependencies = [ leaf-config.ca ];
              script = "tls-signed-certificate";
            };
            group = "tls-leaf";
            mode = "0440";
            settings = {
              root-certificate = leaf-config.ca;
              inherit (leaf-config) fqdn;
            };
            rekeyFile = ../secrets/tls-${leaf-config.fqdn}.key.age;
          };
        })
        tls-leafs
    );
  });

}
