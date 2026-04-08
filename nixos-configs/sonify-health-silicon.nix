{ pkgs, ... }:
{
  environment.systemPackages = [ pkgs.alsa-utils ];

  systemd.services.alsa-unmute = {
    description = "Unmute ALSA master volume";
    wantedBy = [ "sonify-health.service" ];
    before = [ "sonify-health.service" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${pkgs.alsa-utils}/bin/amixer -D hw:0 sset Master 100% unmute";
    };
  };

  services.sonify-health = {
    enable = true;
    logLevel = "debug";
    listen = "/run/sonify-health/sonify-health.sock";
    heartbeat = {
      slot = 2;
      cycleDurationSecs = 14;
      checks = [
        {
          name = "internal";
          command = "${pkgs.fping}/bin/fping -q -t 4000 -r 1 192.168.254.254 192.168.254.6 titanium.proton";
        }
        {
          name = "external";
          command = "${pkgs.fping}/bin/fping -q -t 4000 -r 1 208.67.222.222 9.9.9.9 resolver1.opendns.com api.anthropic.com";
        }
      ];
    };
    drone.metrics = [
      {
        name = "cpu";
        command = "${pkgs.gawk}/bin/awk '{nproc='\"$(${pkgs.coreutils}/bin/nproc)\"'; printf \"%.2f\\n\", $1/nproc}' /proc/loadavg";
        resultMode = "stdout";
        register = "mid";
      }
    ];
  };
}
