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
      detector_cmd = "${config.services.proc-siding.package}/share/proc-siding/detectors/metal-gpu.sh --exclude-pattern ollama";
      action = {
        kind = "http_post";
        pressure_url = "http://127.0.0.1:9091/control/pause";
        clear_url = "http://127.0.0.1:9091/control/resume";
      };
    };
  };
}
