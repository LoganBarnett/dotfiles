################################################################################
# Configure this NixOS host to act as an Airplay server.
#
# There might be difficulty with this on a host that acts a desktop and thus I
# don't necessarily want the main audio hijacked.  This might better be deferred
# to a Pi.
#
# Untested since upgrading to NixOS 25.11.
################################################################################
{ lib, host-id, ... }:
{
  users.users.logan.extraGroups = [ "pipewire" ];
  users.users.shairport.extraGroups = [ "pipewire" ];
  systemd.services.shairport-sync.environment.PULSE_RUNTIME_PATH = "/run/pulse";
  services.pipewire = {
    enable = true;
    systemWide = true;
    pulse.enable = true;
  };
  services.shairport-sync = {
    enable = true;
    settings = {
      general = {
        name = "${host-id}-speakers";
        output_backend = "pa";
      };
      diagnostics.log_verbosity = 3;
    };
    openFirewall = true;
  };
  # Avahi = mDNS/Bonjour. Needed for discovery.
  services.avahi = {
    enable = true;
    # domainName = "${host-id}.proton";
    nssmdns4 = true; # enable .local name resolution (IPv4)
    nssmdns6 = true; # enable .local name resolution (IPv6)
    publish = {
      enable = true; # publish this host
      addresses = true; # include addresses
      workstation = true;
    };
    # If your sender is on a different VLAN, Avahi can reflect mDNS:
    # reflector = true;  # only if you truly need cross-subnet discovery
    openFirewall = true;
    wideArea = true;
  };
}
