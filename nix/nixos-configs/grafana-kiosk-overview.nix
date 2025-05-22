################################################################################
# Display the overview dashboard in Grafana in kiosk mode.
################################################################################
{ ... }: {
  imports = [
    ../nixos-modules/grafana-kiosk.nix
  ];
  services.grafana-kiosk = {
    enable = true;
    url = "https://grafana.proton/d/system-monitoring/system-monitoring?orgId=1&from=now-6h&to=now&timezone=America/Los_Angeles&kiosk=fullscreen&refresh=5s";
  };
}
