################################################################################
# Immich is a self-hosted photo and video backup solution.  This module wraps
# the NixOS immich service with site-specific integration: reverse proxy
# registration, upload size override, and systemd mount ordering.
################################################################################
{
  config,
  lib,
  ...
}:
let
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    types
    ;
  cfg = config.services.immich-host;
in
{
  imports = [ ./https.nix ];

  options.services.immich-host = {
    enable = mkEnableOption "Immich photo/video backup service";

    fqdn = mkOption {
      type = types.str;
      default = "immich.proton";
      description = "FQDN for the HTTPS reverse proxy endpoint.";
    };

    port = mkOption {
      type = types.port;
      default = 2283;
      description = "Internal HTTP port for the Immich server.";
    };

    mediaLocation = mkOption {
      type = types.path;
      description = "Directory where Immich stores uploaded media.";
    };

    machineLearning = mkOption {
      type = types.bool;
      default = true;
      description = "Enable the Immich machine-learning sidecar (smart search, face recognition).";
    };

    mountDependencies = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = ''
        Systemd units that must be active before Immich starts.  Listed in
        both <code>after</code> and <code>requires</code>.
      '';
    };
  };

  config = mkIf cfg.enable {
    services.https.fqdns.${cfg.fqdn} = {
      internalPort = cfg.port;
    };

    services.immich = {
      enable = true;
      port = cfg.port;
      mediaLocation = cfg.mediaLocation;
      machine-learning.enable = cfg.machineLearning;
    };

    # Allow arbitrarily large uploads; Immich handles multi-gigabyte video
    # files and nginx's default 1 MB body limit would reject them.
    services.nginx.virtualHosts.${cfg.fqdn}.extraConfig = ''
      client_max_body_size 0;
    '';

    systemd.services.immich-server = {
      after = cfg.mountDependencies;
      requires = cfg.mountDependencies;
    };

    systemd.services.immich-machine-learning = mkIf cfg.machineLearning {
      after = cfg.mountDependencies;
      requires = cfg.mountDependencies;
    };
  };
}
