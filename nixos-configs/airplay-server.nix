################################################################################
# Configure this NixOS host to act as an Airplay server.
#
# There might be difficulty with this on a host that acts a desktop and thus I
# don't necessarily want the main audio hijacked.  This might better be deferred
# to a Pi.
#
# Untested since upgrading to NixOS 25.11.
################################################################################
{ lib, host-id, ... }: {
  disabledModules = [
    "services/networking/shairport-sync.nix"
    "services/desktops/pipewire/pipewire.nix"
    # "services/audio/pulseaudio.nix"
  ];
  imports = [
    # Doesn't work due to conflicts between nixpkgs versions.
    # "${flake-inputs.nixpkgs-latest}/nixos/modules/services/networking/shairport-sync.nix"
    ../nixos-modules/shairport-sync.nix
    ../nixos-modules/pipewire.nix
    # ../nixos-modules/pulseaudio.nix
  ];

  users.users.logan.extraGroups = [ "pipewire" ];
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
  # services.pipewire.pulse.systemWide = true;
  # Avahi = mDNS/Bonjour. Needed for discovery.
  services.avahi = {
    enable = true;
    # domainName = "${host-id}.proton";
    nssmdns4 = true;     # enable .local name resolution (IPv4)
    nssmdns6 = true;     # enable .local name resolution (IPv6)
    publish = {
      enable = true;     # publish this host
      addresses = true;  # include addresses
      workstation = true;
    };
    # If your sender is on a different VLAN, Avahi can reflect mDNS:
    # reflector = true;  # only if you truly need cross-subnet discovery
    openFirewall = true;
    wideArea = true;
  };
}
