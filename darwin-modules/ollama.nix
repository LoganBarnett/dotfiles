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

    loadModels = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = ''
        Pull these models via `ollama pull` after the server starts.

        A separate launchd user agent runs once at login, waits for the
        server to accept connections, then pulls each model in order.
        Models already present are skipped by ollama automatically.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    # Liveness check — confirm Ollama is running and responding to HTTP
    # requests.  This catches the process-completely-down case that the GPU
    # eviction check (goss-ollama-metal-gpu.nix) cannot detect because it
    # passes when no models are loaded.
    services.goss.checks.http."http://localhost:${toString cfg.port}/api/tags" = {
      status = 200;
      timeout = 5000;
    };

    system.activationScripts.postActivation.text = ''
      mkdir --parents /var/log/ollama
      chown "$(/usr/bin/stat -f '%Su' /dev/console)" /var/log/ollama
      chmod 755 /var/log/ollama
    '';

    # Rotate Ollama and model-loader logs via macOS newsyslog.
    # N = no signal (Ollama isn't syslogd; it re-opens the file on next write).
    # J = bzip2 compression for rotated files.
    environment.etc."newsyslog.d/ollama.conf".text = ''
      # logfilename                          mode count size  when flags
      /var/log/ollama/ollama.log              644  5     10240 *    NJ
      /var/log/ollama/model-loader.log        644  3     1024  *   NJ
    '';

    launchd.user.agents.ollama-model-loader = lib.mkIf (cfg.loadModels != [ ]) {
      serviceConfig = {
        RunAtLoad = true;
        KeepAlive = false;
        ProgramArguments = [
          "${pkgs.writeShellScript "ollama-model-loader" ''
            set -euo pipefail
            echo "[$(date -Iseconds)] model-loader starting"
            # Wait for the ollama server to accept connections before pulling.
            until ${cfg.package}/bin/ollama list >/dev/null 2>&1; do
              echo "[$(date -Iseconds)] waiting for ollama server..."
              sleep 2
            done
            echo "[$(date -Iseconds)] ollama server ready"
            ${lib.concatMapStrings (model: ''
              echo "[$(date -Iseconds)] pulling ${model}..."
              if ${cfg.package}/bin/ollama pull ${lib.escapeShellArg model}; then
                echo "[$(date -Iseconds)] pulled ${model} successfully"
              else
                echo "[$(date -Iseconds)] ERROR: failed to pull ${model} (exit $?)"
              fi
            '') cfg.loadModels}
            echo "[$(date -Iseconds)] model-loader complete"
          ''}"
        ];
        StandardOutPath = "/var/log/ollama/model-loader.log";
        StandardErrorPath = "/var/log/ollama/model-loader.log";
      };
    };

    launchd.user.agents.ollama = {
      path = [ config.environment.systemPath ];

      serviceConfig = {
        KeepAlive = true;
        RunAtLoad = true;
        # Without this, macOS defaults to "Background" which enables App Nap
        # and I/O throttling.  That can cause the OS to deprioritize Ollama
        # and evict its Metal GPU context when the screen is locked or under
        # memory pressure.
        ProcessType = "Interactive";
        ProgramArguments = [
          "${cfg.package}/bin/ollama"
          "serve"
        ];

        StandardOutPath = "/var/log/ollama/ollama.log";
        StandardErrorPath = "/var/log/ollama/ollama.log";

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
