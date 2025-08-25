################################################################################
# A module that allows easily expressing environment files for secrets.
################################################################################
{ config, lib, lib-custom, pkgs, ... }: let
  inherit (lib) mkEnableOption mkMerge mkOption types;
  inherit (lib.attrsets) foldlAttrs mapAttrs mapAttrs' mapAttrsToList;
  cfg = config.services.environment-file-secrets;
in {
  options.services.environment-file-secrets = {
    services = mkOption {
      type = types.attrsOf (types.submodule {
        options = {
          enable = (
            mkEnableOption "Enable environment file secrets for this service."
          ) // {
            default = true;
          };

          secrets = mkOption {
            type = types.attrsOf types.str;
            default = {};
            description = ''
              An attrset of secret names from age.secrets to environment
              variables.
            '';
          };
        };
      });
      description = "Services that use environment files for secrets.";
      default = {};
    };
  };

  config = {

    age.secrets =
      foldlAttrs
        (acc: serviceName: value: let
          service = cfg.services.${serviceName};
        in acc
           // {
             "${serviceName}-environment-file" = {
               generator = {
                 script = "environment-file";
                 dependencies = builtins.map (name:
                   config.age.secrets."${name}-environment-variable"
                 ) (builtins.attrNames service.secrets);
               };
             };
           }
        // (mapAttrs' (secretName: envName: {
          name = "${secretName}-environment-variable";
          value = {
            generator = {
              script = "environment-variable";
              dependencies = [
                config.age.secrets.${secretName}
              ];
            };
            settings.field = envName;
          };
        }) service.secrets)
        )
        {}
        cfg.services
    ;

    systemd.services = mapAttrs (serviceName: value: let
      after = [ "run-agenix.d.mount" ];
    in {
      inherit after;
      requires = after;
      serviceConfig = {
        ImportCredential = [
          "secrets.env"
        ];
        LoadCredential = [
          "secrets.env:${
            config
              .age
              .secrets
              ."${serviceName}-environment-file"
              .path
          }"
        ];
      };
    }) cfg.services;

  };
}
