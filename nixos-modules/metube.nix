################################################################################
# Metube is a web frontend for yt-dlp.  This module wraps the metube package
# with site-specific integration: reverse proxy registration, systemd mount
# ordering, and media group membership.
################################################################################
{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    types
    ;
  cfg = config.services.metube-host;
in
{
  imports = [ ./https.nix ];

  options.services.metube-host = {
    enable = mkEnableOption "Metube web frontend for yt-dlp";

    package = mkOption {
      type = types.package;
      default = pkgs.metube;
      description = "The metube package to use.";
    };

    port = mkOption {
      type = types.port;
      default = 8090;
      description = "Internal HTTP port for the metube service.";
    };

    downloadDir = mkOption {
      type = types.path;
      description = "Directory where downloaded media is stored.";
    };

    fqdn = mkOption {
      type = types.str;
      default = "metube.proton";
      description = "FQDN for the HTTPS reverse proxy endpoint.";
    };

    mountDependencies = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = ''
        Systemd units that must be active before metube starts.  Listed in
        both <code>after</code> and <code>requires</code>.
      '';
    };

    extraGroups = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "Additional groups for the metube service user.";
    };
  };

  config = mkIf cfg.enable {
    services.https.fqdns.${cfg.fqdn} = {
      enable = true;
      internalPort = cfg.port;
    };

    users.groups.metube = { };

    users.users.metube = {
      isSystemUser = true;
      group = "metube";
      extraGroups = cfg.extraGroups;
    };

    systemd.services.metube = {
      description = "MeTube web frontend for yt-dlp";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ] ++ cfg.mountDependencies;
      requires = cfg.mountDependencies;
      environment = {
        DOWNLOAD_DIR = cfg.downloadDir;
        HOST = "127.0.0.1";
        PORT = toString cfg.port;
        STATE_DIR = "/var/lib/metube";
      };
      serviceConfig = {
        ExecStart = lib.getExe cfg.package;
        User = "metube";
        Group = "metube";
        StateDirectory = "metube";
        Restart = "on-failure";
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ReadWritePaths = [
          cfg.downloadDir
          "/var/lib/metube"
        ];
      };
    };
  };
}
