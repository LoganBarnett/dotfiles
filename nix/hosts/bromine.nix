################################################################################
# Bromine is a trace element that is highly reactive and thus isn't found freely
# in nature.  It was used long ago as a sedative and can help people with
# epilepsy.
################################################################################
{ host-id, system, ... }: {
  imports = [
    ../nixos-modules/raspberry-pi-4.nix
    # ../nixos-modules/nix-builder-provide.nix
    # This seems to fail with the generated key.
    # ../nixos-modules/raspberry-pi-builder.nix
    ../nixos-modules/server-host.nix
    (import ../nixos-modules/kiosk-url.nix {
      url = "https://grafana.proton/d/system-monitoring/system-monitoring?orgId=1&from=now-6h&to=now&timezone=America/Los_Angeles&kiosk=fullscreen&refresh=5s";
      browser = "chromium";
    })
    {
      networking.hostId = "37fab2ca";
      nixpkgs.hostPlatform = system;
    }
    (import ../nixos-modules/https.nix {
      server-port = 3000;
      inherit host-id;
      fqdn = "${host-id}.proton";
    })
  ];
}
