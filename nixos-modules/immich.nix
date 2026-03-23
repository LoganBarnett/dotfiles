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
      # This host's stateVersion predates 25.11, so the NixOS module would
      # otherwise default to enabling pgvecto.rs (vectors extension).  Immich
      # 2.x has migrated to VectorChord, so disable the legacy extension
      # explicitly.  The NixOS module sets up VectorChord as superuser via
      # postgresql-setup before Immich starts, so Immich never needs to alter
      # extensions itself.
      database.enableVectors = false;
    };

    # The NixOS immich module only manages the default /var/lib/immich path via
    # tmpfiles.  Non-default paths must be created declaratively so the immich
    # user has ownership before the service starts.
    systemd.tmpfiles.rules = [
      "d ${cfg.mediaLocation} 0700 immich immich -"
    ];

    # Allow arbitrarily large uploads; Immich handles multi-gigabyte video
    # files and nginx's default 1 MB body limit would reject them.
    services.nginx.virtualHosts.${cfg.fqdn}.extraConfig = ''
      client_max_body_size 0;
    '';

    systemd.services.immich-server = {
      after = cfg.mountDependencies;
      requires = cfg.mountDependencies;
      # Seed an admin user with an empty (permanently unusable) password if no
      # admin exists yet.  This bypasses the first-run wizard.  The password
      # field defaults to '' which no bcrypt comparison will ever match, so
      # local password login is dead on arrival.  OIDC populates oauthId on
      # first login and is the intended authentication path.
      postStart = ''
        ${lib.getExe' config.services.postgresql.package "psql"} \
          --dbname=immich \
          --no-password \
          --command='
            INSERT INTO "user" (email, name, "isAdmin")
            SELECT '"'"'admin@proton'"'"', '"'"'Admin'"'"', true
            WHERE NOT EXISTS (SELECT 1 FROM "user" WHERE "isAdmin" = true);
          '
      '';
    };

    systemd.services.immich-machine-learning = mkIf cfg.machineLearning {
      after = cfg.mountDependencies;
      requires = cfg.mountDependencies;
      # Silicon had ample headroom (7.6 GiB RAM, ~5.6 GiB free, load ~0.00)
      # at time of writing.  These are safeguards so a large initial library
      # scan cannot crowd out other services on this shared host.
      serviceConfig = {
        CPUWeight = 20;
        IOWeight = 20;
        Nice = 10;
        MemoryHigh = "2G";
      };
    };
  };
}
