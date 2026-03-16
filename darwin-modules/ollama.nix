################################################################################
# Darwin launchd module for Ollama.
#
# Copied from https://github.com/nix-darwin/nix-darwin/pull/972 (not yet
# merged as of 2025-07-14).  Once that PR lands in our pinned nix-darwin,
# remove this file and rely on the upstream module instead.
#
# Ollama runs as a user agent (launchd.user.agents) rather than a system
# daemon so it can access the logged-in user's Metal GPU context.
################################################################################
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.ollama;

in
{
  options.services.ollama = {
    enable = lib.mkEnableOption "Ollama inference server";

    package = lib.mkPackageOption pkgs "ollama" { };

    host = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1";
      example = "0.0.0.0";
      description = ''
        Host address the Ollama HTTP interface listens on.
      '';
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 11434;
      example = 11111;
      description = ''
        Port the Ollama HTTP interface listens on.
      '';
    };

    models = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "/path/to/ollama/models";
      description = ''
        Directory Ollama reads models from and downloads new models to.
      '';
    };

    environment = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      example = {
        OLLAMA_LLM_LIBRARY = "cpu";
        HIP_VISIBLE_DEVICES = "0,1";
      };
      description = ''
        Environment variables passed to the Ollama server process.

        These are only visible to the launchd user agent, not to ad-hoc
        invocations like `ollama run`.  Since `ollama run` shells out to the
        server for actual inference, this is usually sufficient.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    launchd.user.agents.ollama = {
      path = [ config.environment.systemPath ];

      serviceConfig = {
        KeepAlive = true;
        RunAtLoad = true;
        ProgramArguments = [
          "${cfg.package}/bin/ollama"
          "serve"
        ];

        EnvironmentVariables =
          cfg.environment
          // {
            OLLAMA_HOST = "${cfg.host}:${toString cfg.port}";
          }
          // lib.optionalAttrs (cfg.models != null) {
            OLLAMA_MODELS = cfg.models;
          };
      };
    };
  };
}
