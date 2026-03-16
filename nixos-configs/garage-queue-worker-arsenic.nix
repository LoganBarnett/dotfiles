{ config, ... }:
{
  services.garage-queue-worker = {
    enable = true;
    settings = {
      worker = {
        server_url = "http://silicon.proton:9090";
        poll_interval_ms = 1000;
      };
      control = {
        host = "127.0.0.1";
        port = 9091;
      };
      capabilities = {
        tags = config.services.ollama.loadModels;
        scalars.vram_mb = 8192;
      };
      delegator = {
        kind = "http";
        url = "http://127.0.0.1:11434/api/generate";
      };
    };
  };
}
