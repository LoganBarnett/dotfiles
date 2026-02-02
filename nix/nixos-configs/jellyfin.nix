################################################################################
# Jellyfin is a free and open-source media server application.
#
# This configuration sets up Jellyfin to serve media from NFS-mounted volumes
# that are provided by the nfs-provider infrastructure.
################################################################################
{ config, host-id, lib, pkgs, ... }: let
  web-port = 8096;
in {
  imports = [
    ../nixos-modules/nfs-consumer-facts.nix
    ../nixos-modules/nfs-mount-consumer.nix
  ];

  nfsConsumerFacts = {
    enable = true;
    provider = {
      remoteHost = "silicon.proton";
      vpnHost = "silicon-nas.proton";
      providerHostId = "silicon";
      wgPort = 51821;
    };
  };

  services.https.fqdns."jellyfin.proton" = {
    enable = true;
    internalPort = web-port;
  };
  services.jellyfin = {
    enable = true;
    # The default user is "jellyfin" with a dynamic UID/GID.
    # We'll ensure the jellyfin user can access the media directories.
    # dataDir defaults to /var/lib/jellyfin for config and database.
    # Media files will be served from /mnt/jellyfin-media (NFS mount).
    openFirewall = false;
  };

  # Declaratively configure Jellyfin media library via API on startup.
  systemd.services.jellyfin-setup-library = {
    description = "Configure Jellyfin media library";
    after = [ "jellyfin.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    script = ''
      # Wait for Jellyfin to be ready.
      for i in {1..30}; do
        ${pkgs.curl}/bin/curl -s http://localhost:${toString web-port}/health && break
        sleep 2
      done

      # TODO: Use Jellyfin API to add /mnt/jellyfin-media as a media library.
      # This requires authentication and proper API calls.
      # For now, this is a placeholder for the declarative configuration.
      echo "Jellyfin library setup would go here"
    '';
  };

  # Ensure the jellyfin user can access NFS-mounted media.
  # The actual media path will be available via the nfsConsumerFacts module
  # at /mnt/jellyfin-media (or whatever volume name is configured in facts.nix).
  users.users.jellyfin = {
    # This will be set up by the service, but we ensure it exists.
    isSystemUser = true;
    group = "jellyfin";
    extraGroups = [
      # Allow jellyfin to read from the media mount point.
      "jellyfin"
      # Add the jellyfin user to the render group for hardware acceleration.
      "render"
      "video"
    ];
  };

  users.groups.jellyfin = {};

  # Enable hardware acceleration if available (for transcoding).
  # This is especially useful for Intel QuickSync on the Mac Mini.
  # Note: May need to adjust based on actual hardware capabilities.
  hardware.graphics.enable = true;

  networking.firewall.allowedUDPPorts = [
    1900  # DLNA discovery.
    7359  # Jellyfin auto-discovery.
  ];
}
