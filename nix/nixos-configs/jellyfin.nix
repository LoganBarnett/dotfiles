################################################################################
# Jellyfin is a free and open-source media server application.
#
# This configuration sets up Jellyfin to serve media from NFS-mounted volumes
# that are provided by the nfs-provider infrastructure.
################################################################################
{ config, host-id, lib, pkgs, ... }: let
  dataDir = "/mnt/jellyfin-data";
in {
  imports = [
    ../nixos-modules/nfs-consumer-facts.nix
    ../nixos-modules/nfs-mount-consumer.nix
  ];
  services.jellyfin = {
    enable = true;
    inherit dataDir;
    # The default user is "jellyfin" with a dynamic UID/GID.
    # We'll ensure the jellyfin user can access the media directories.
    openFirewall = true;
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

  # Nginx reverse proxy configuration could go here if needed,
  # but for now we'll keep it simple and access via port 8096.

  networking.firewall.allowedTCPPorts = [
    8096  # Jellyfin web interface.
    8920  # Jellyfin HTTPS (optional).
  ];

  networking.firewall.allowedUDPPorts = [
    1900  # DLNA discovery.
    7359  # Jellyfin auto-discovery.
  ];
}
