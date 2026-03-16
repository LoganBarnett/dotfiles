################################################################################
# Display the overview dashboard in Grafana in kiosk mode.
################################################################################
{
  config,
  facts,
  lib,
  ...
}:
{
  imports = [
    ../nixos-modules/grafana-kiosk.nix
  ];
  services.grafana-kiosk = {
    enable = true;
    url = "https://grafana.proton/d/system-monitoring/system-monitoring?orgId=1&from=now-6h&to=now&timezone=America/Los_Angeles&kiosk=fullscreen&refresh=1m";
  };
  # Not sure if this is doing anything.
  services.xserver.displayManager.sessionCommands = ''
    xrandr --output eDP-1 --brightness 2.0
  '';
  # Print the manual restart command on every deploy as a reminder, since
  # restartTriggers does not yet fire for user services (see below).
  system.activationScripts.grafana-kiosk-restart-hint = ''
    echo "To restart the Grafana kiosk manually, run:"
    echo "  sudo -u ${config.services.grafana-kiosk.systemUser} XDG_RUNTIME_DIR=/run/user/\$(id -u ${config.services.grafana-kiosk.systemUser}) systemctl --user restart grafana-kiosk.service"
  '';
  # Restart the kiosk when dashboards change, using the same hash that
  # grafana.nix uses, so both services restart together on deploy.
  #
  # NOTE: As of 2026-02-26 this does not work.  NixOS's switch-to-configuration
  # script only applies restartTriggers logic to system-scoped units; user
  # services receive only a daemon-reexec.  This is a known missing feature:
  # https://github.com/NixOS/nixpkgs/issues/246611
  # Left in place so it takes effect if/when that issue is resolved.
  systemd.user.services.grafana-kiosk.restartTriggers = [
    (builtins.hashString "sha256" (
      builtins.toJSON (
        facts.network.monitoring.grafanaDashboards { inherit lib facts; }
      )
    ))
  ];
}
