{ config, ... }:
{
  services.proc-siding = {
    enable = true;
    settings = {
      pressure = {
        threshold = 25;
        # 12 × 5 s = 60 s of sustained pressure required before pausing.
        hysteresis = 12;
        poll_interval_ms = 5000;
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
      # Mute sonify-health while GPU is under non-inference load.
      extra_actions = [
        {
          kind = "http";
          pressure_url = "unix:/run/sonify-health/sonify-health.sock:/api/mute";
          pressure_method = "PUT";
          clear_url = "unix:/run/sonify-health/sonify-health.sock:/api/mute";
          clear_method = "DELETE";
        }
      ];
    };
  };
}
