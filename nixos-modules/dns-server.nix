################################################################################
# DNS/DHCP Server Integration Module.
#
# This module integrates Blocky (DNS ad-blocking/classification) with dnsmasq
# (DHCP + local hostname resolution) to provide a complete DNS/DHCP solution.
#
# Architecture:
# - Blocky (port 53): Primary DNS for all clients with ad-blocking
# - dnsmasq (port 5353): Local hostname resolution for .proton domain
# - dnsmasq (port 67): DHCP server
#
# This module handles:
# - Importing and configuring both services
# - Ensuring proper startup order (dnsmasq before blocky)
# - Integration testing via goss to verify the services work together
################################################################################
{ host-id, facts, ... }: {
  imports = [
    ../nixos-configs/blocky-with-updater.nix
    ../nixos-modules/dhcp-server.nix
  ];

  # Ensure blocky starts after dnsmasq since it queries dnsmasq for local
  # hostname resolution.
  systemd.services.blocky = {
    after = [ "dnsmasq.service" ];
    wants = [ "dnsmasq.service" ];
  };

  # Integration goss checks: Verify blocky and dnsmasq work together.
  services.goss.checks = {
    # Test end-to-end DNS resolution through blocky on standard port 53.
    # Blocky should forward local domain queries to dnsmasq on port 5353.
    dns."${host-id}.${facts.network.domain}" = {
      resolvable = true;
      server = "127.0.0.1";
      timeout = 3000;
    };
    # Verify the HTTPS admin interface is accessible (handled by reverse proxy).
    port."tcp:443" = {
      listening = true;
      ip = [ "0.0.0.0" ];
    };
  };
}
