################################################################################
# Darwin equivalent of nixpkgs's nixos/modules/services/monitoring/goss.nix.
#
# Uses launchd instead of systemd.  Option names and types mirror the NixOS
# module so the two stay easy to compare and keep in sync.
################################################################################
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.goss;

  settingsFormat = pkgs.formats.yaml { };
  configFile = settingsFormat.generate "goss.yaml" cfg.checks;

in
{
  options.services.goss = {
    enable = lib.mkEnableOption "Goss daemon";

    package = lib.mkPackageOption pkgs "goss" { };

    environment = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      example = {
        GOSS_FMT = "prometheus";
        GOSS_LOGLEVEL = "FATAL";
        GOSS_LISTEN = ":8080";
      };
      description = ''
        Environment variables passed to the goss process.

        See <https://github.com/goss-org/goss/blob/master/docs/manual.md>.
      '';
    };

    extraArgs = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [
        "--format-options"
        "verbose"
      ];
      description = ''
        Extra arguments appended to the goss serve invocation.
      '';
    };

    checks = lib.mkOption {
      type = lib.types.submodule { freeformType = settingsFormat.type; };
      default = { };
      example = {
        addr."tcp://localhost:8080" = {
          reachable = true;
          local-address = "127.0.0.1";
        };
        service.goss = {
          enabled = true;
          running = true;
        };
      };
      description = ''
        Goss check definitions written to goss.yaml.

        Refer to <https://github.com/goss-org/goss/blob/master/docs/goss-json-schema.yaml>
        for the schema.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    launchd.daemons.goss = {
      serviceConfig = {
        Label = "org.goss.goss";
        # GOSS_USE_ALPHA is required on macOS: goss treats the serve subcommand
        # as alpha on non-Linux platforms and refuses to start without it.
        EnvironmentVariables = {
          GOSS_FILE = "${configFile}";
          GOSS_USE_ALPHA = "1";
        }
        // cfg.environment;
        ProgramArguments = [
          "${cfg.package}/bin/goss"
          "serve"
        ]
        ++ cfg.extraArgs;
        RunAtLoad = true;
        KeepAlive = true;
        StandardOutPath = "/var/log/goss.log";
        StandardErrorPath = "/var/log/goss.log";
      };
    };
  };
}
