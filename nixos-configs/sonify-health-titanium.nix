{ config, pkgs, ... }:
{
  services.sonify-health = {
    enable = true;
    listen = "/run/sonify-health/sonify-health.sock";
    heartbeat = {
      slot = 1;
      cycleDurationSecs = 8;
      checks = [
        {
          name = "internal";
          command = "${pkgs.fping}/bin/fping -q -t 4000 -r 1 192.168.254.254 192.168.254.9 silicon.proton";
        }
        {
          name = "external";
          command = "${pkgs.fping}/bin/fping -q -t 4000 -r 1 208.67.222.222 9.9.9.9 resolver1.opendns.com api.anthropic.com";
        }
      ];
    };
    drone.metrics = [
      {
        name = "gpu";
        command = "${pkgs.gawk}/bin/awk 'BEGIN{getline v < \"${config.hardware.amdGpuCard.sysfsPath}/gpu_busy_percent\"; printf \"%.2f\\n\", v/100}'";
        resultMode = "stdout";
        register = "low";
      }
    ];
  };
  # Create /run/sonify-health/ for the Unix socket.
  systemd.services.sonify-health.serviceConfig.RuntimeDirectory = "sonify-health";
}
