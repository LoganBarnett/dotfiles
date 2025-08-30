################################################################################
# A module that allows easily expressing environment files for secrets.
################################################################################
{ config, lib, lib-custom, pkgs, ... }: let
  inherit (lib) mkEnableOption mkMerge mkOption types;
  inherit (lib.attrsets) foldlAttrs mapAttrs mapAttrs' mapAttrsToList;
  cfg = config.services.environment-file-secrets;
in {
  options.services.environment-file-secrets = let
    serviceSubmodule = types.submodule {
      options = {
        enable = (
          mkEnableOption "Enable environment file secrets for this service."
        ) // {
          default = true;
        };

        secrets = mkOption {
          type = types.attrsOf secretSubmodule;
          default = {};
          description = ''
            An attrset of secret names from age.secrets to environment
            variables.
          '';
        };
      };
    };
    secretSubmodule = types.submodule ({ name, config, ... }: {
      options = {
        environmentVariable = mkOption {
          type = types.str;
          default = config.secretName;
        };
        secretName = mkOption {
          type = types.str;
          default = "${name}-environment-variable";
          description = ''
            The name of the secret for the environment-variable.  Defaults to
            `name` but could be different for greater control (such as two hosts
            that want the same environment variable).
          '';
        };
        rekeyFile = mkOption {
          type = types.nullOr types.path;
          default = null;
        };
      };
    });

  in {
    services = mkOption {
      type = types.attrsOf serviceSubmodule;
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
                 dependencies = mapAttrsToList (name: secret:
                   config.age.secrets."${secret.secretName}"
                 ) service.secrets;
               };
             };
           }
        // (mapAttrs' (name: secret: {
          name = secret.secretName;
          value = {
            generator = {
              script = "environment-variable";
              dependencies = [
                config.age.secrets.${name}
              ];
            };
            settings.field = secret.environmentVariable;
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
        # %N should work here, but it doesn't seem to.
        EnvironmentFile = config
          .age
          .secrets
          ."${serviceName}-environment-file"
          .path
        ;
        # ImportCredential = [
        #   "secrets.env"
        # ];
        # LoadCredential = [
        #   "secrets.env:${
        #     config
        #       .age
        #       .secrets
        #       ."${serviceName}-environment-file"
        #       .path
        #   }"
        # ];
      };
    }) cfg.services;

  };
}
