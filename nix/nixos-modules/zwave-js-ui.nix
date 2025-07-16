{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    getExe
    mkIf
    mkEnableOption
    mkOption
    mkPackageOption
    types
    ;
  cfg = config.services.zwave-js-ui;
in
{
  options.services.zwave-js-ui = {
    enable = mkEnableOption "zwave-js-ui";

    package = mkPackageOption pkgs "zwave-js-ui" { };

    serialPort = mkOption {
      type = types.path;
      description = ''
        Serial port for the Z-Wave controller.

        Only used to grant permissions to the device; must be additionally configured in the application
      '';
      example = "/dev/serial/by-id/usb-example";
    };

    # Copied and adapted from the same thing in zwave-js.
    secretsConfigFile = lib.mkOption {
      type = lib.types.path;
      description = ''
        JSON file containing secret keys. A dummy example:

        ```
        {
          "zwave": {
            "securityKeys": {
              "S0_Legacy": "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
              "S2_Unauthenticated": "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB",
              "S2_Authenticated": "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC",
              "S2_AccessControl": "DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD"
            }
            "securityKeysLongRange": {
              "S2_Authenticated": "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC",
              "S2_AccessControl": "DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD"
            }
          }
        }
        ```

        See
        <https://zwave-js.github.io/node-zwave-js/#/getting-started/security-s2>
        for details. This file will be merged with the module-generated config
        file (taking precedence).

        Z-Wave keys can be generated with:

          {command}`< /dev/urandom tr -dc A-F0-9 | head -c32 ;echo`


        ::: {.warning}
        A file in the nix store should not be used since it will be readable to
        all users.
        :::
      '';
      example = "/secrets/zwave-js-keys.json";
    };

    # Ugh why did environment variables get named "settings" when there is a
    # "settings.json" file we _really_ want to have controlled?
    settings2 = mkOption {
      type = types.submodule {
        freeformType = lib.types.attrsOf lib.types.anything;
        options = {
          logLevel = mkOption {
            type = (types.enum [
              # In order of ascending verbosity.
              "error"
              "warn"
              "info"
              "verbose"
              "debug"
              "silly"
            ]);
            default = "info";
          };
          # Should be serial.port, so this doesn't work.
          # serialPort = mkOption {
          #   type = types.str;
          # };
        };
      };
    };

    # Ugh why did environment variables get named "settings" when there is a
    # "settings.json" file we _really_ want to have controlled?
    settings = mkOption {
      type = types.submodule {
        freeformType =
          with types;
          attrsOf (
            nullOr (oneOf [
              str
              path
              package
            ])
          );

        options = {
          STORE_DIR = mkOption {
            type = types.str;
            default = "%S/zwave-js-ui";
            visible = false;
            readOnly = true;
          };

          ZWAVEJS_EXTERNAL_CONFIG = mkOption {
            type = types.str;
            default = "%S/zwave-js-ui/.config-db";
            visible = false;
            readOnly = true;
          };
        };
      };

      description = ''
        Extra environment variables passed to the zwave-js-ui process.

        Check <https://zwave-js.github.io/zwave-js-ui/#/guide/env-vars> for possible options
      '';
      example = {
        HOST = "::";
        PORT = "8091";
      };
    };
  };
  config = mkIf cfg.enable {
    systemd.services.zwave-js-ui = let
      settingsFile = (pkgs.formats.json {}).generate
        "settings.json"
        cfg.settings2
      ;
      settingsPath = "%S/zwave-js-ui/settings.json";
    in {
      environment = cfg.settings;
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = getExe cfg.package;
        ExecStartPre = [
          # Merge the secrets file and the settings file together.
          (lib.strings.concatStrings [
            "${pkgs.runtimeShell} -c "
            ''"${pkgs.jq}/bin/jq --slurp ''
            "'.[0] * .[1]' ${settingsFile} ${cfg.secretsConfigFile} "
            ''  > ${settingsPath}"''
          ])
          ''
            ${pkgs.coreutils}/bin/chmod 600 ${settingsPath}
          ''
        ];
        RuntimeDirectory = "zwave-js-ui";
        StateDirectory = "zwave-js-ui";
        RootDirectory = "%t/zwave-js-ui";
        BindReadOnlyPaths = [
          "/nix/store"
        ];
        DeviceAllow = [ cfg.serialPort ];
        DynamicUser = true;
        SupplementaryGroups = [ "dialout" ];
        CapabilityBoundingSet = [ "" ];
        RestrictAddressFamilies = "AF_INET AF_INET6";
        DevicePolicy = "closed";
        LockPersonality = true;
        MemoryDenyWriteExecute = false;
        NoNewPrivileges = true;
        PrivateUsers = true;
        PrivateTmp = true;
        ProtectClock = true;
        ProtectControlGroups = true;
        ProtectHome = true;
        ProtectHostname = true;
        ProtectKernelLogs = true;
        ProtectKernelModules = true;
        # Pending: https://github.com/NixOS/nixpkgs/pull/418537
        ProtectKernelTunables = true;
        ProtectProc = "invisible";
        ProcSubset = "pid";
        RemoveIPC = true;
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        SystemCallArchitectures = "native";
        SystemCallFilter = [
          "@system-service @pkey"
          "~@privileged @resources"
          "@chown"
        ];
        UMask = "0077";
        WorkingDirectory = "%S/zwave-js-ui";
      };
    };
  };
  meta.maintainers = with lib.maintainers; [ cdombroski ];
}
