{ lib, pkgs, config, ... }:
let
  inherit (lib) mkEnableOption mkOption mkIf types concatStringsSep
  mapAttrsToList optionalString;
  cfg = config.services.chronicle-proxy;
  serviceName = "chronicle-proxy";

  # Render extra args (long-form preferred; if you must pass short flags, add
  # comments).
  cliArgs = concatStringsSep " " cfg.extraArgs;
in
{
  options.services.${serviceName} = {
    enable = mkEnableOption "Chronicle OpenAI-compatible proxy daemon";

    package = mkOption {
      type = types.package;
      default = pkgs.chronicle-proxy;
      description = "The chronicle-proxy package to run.";
    };

    environment = mkOption {
      type = types.attrsOf types.str;
      default = { };
      description = "Environment variables passed directly to the service.";
      example = {
        CHRONICLE_LISTEN_ADDR = "0.0.0.0:11434";
        RUST_LOG = "info,chronicle=debug";
      };
    };

    credentialFiles = mkOption {
      type = types.attrsOf types.str;
      default = { };
      description = ''
        Map of credential filename → source path on the host.
        Example: { db_url = config.age.secrets.chronicle-db-url.path; openai_key
        = config.age.secrets.openai-key.path; }
      '';
    };

    extraArgs = mkOption {
      type = types.listOf types.str;
      default = [ ];
      example = [
        # "--address" "0.0.0.0"   # if the binary supports it
        # "--port" "11434"        # if the binary supports it
      ];
      description = "Extra CLI arguments passed to chronicle (long form preferred).";
    };

    dataDir = mkOption {
      type = types.path;
      default = "/var/lib/chronicle";
      description = "State directory (used as working directory).";
    };

    host = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = "IP host to bind to";
    };

    port = mkOption {
      type = types.port;
      default = 9782;
      description = "TCP port to listen on";
    };

  };

  config = mkIf cfg.enable {

    systemd.services.${serviceName} = {
      description = "Chronicle OpenAI-compatible proxy";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];

      environment = cfg.environment;
      serviceConfig = {
        Type = "simple";
        ExecStart = ''
          ${lib.getExe cfg.package} \
            --host ${lib.escapeShellArg cfg.host} \
            --port ${lib.escapeShellArg (toString cfg.port)} \
            ${cliArgs}
        '';
        WorkingDirectory = cfg.dataDir;
        StateDirectory = "chronicle";
        StateDirectoryMode = "0750";
        RuntimeDirectory = "chronicle";
        RuntimeDirectoryMode = "0750";
        # Hardening
        AmbientCapabilities = "";
        CapabilityBoundingSet = "";
        DynamicUser = true;
        PrivateTmp = true;
        ProtectHome = true;
        ProtectSystem = "strict";
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        NoNewPrivileges = true;
        RestrictAddressFamilies = [ "AF_UNIX" "AF_INET" "AF_INET6" ];
        RestrictRealtime = true;
        LockPersonality = true;
        SystemCallArchitectures = "native";
        SystemCallFilter = [ "@system-service" "~@privileged" "~@resources" ];
        ReadWritePaths = [ cfg.dataDir ];
      };
    };
  };
}
