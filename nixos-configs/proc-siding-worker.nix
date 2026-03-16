{ config, ... }:
{
  services.proc-siding = {
    enable = true;
    settings = {
      pressure = {
        threshold = 25;
        hysteresis = 3;
        poll_interval_ms = 2000;
      };
      process_discovery = {
        kind = "systemd_unit";
        unit = "ollama.service";
      };
      action = {
        kind = "http_post";
        pressure_url = "http://127.0.0.1:9091/control/pause";
        clear_url = "http://127.0.0.1:9091/control/resume";
      };
    };
  };
}
