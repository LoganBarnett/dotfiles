{ ... }:
{
  services.garage-queue-worker.workers.ollama = {
    enable = true;
    integrations.ollama.enable = true;
    settings = {
      worker = {
        server_url = "https://ollama.proton";
        poll_interval_ms = 1000;
      };
      control = {
        host = "127.0.0.1";
        port = 9091;
      };
    };
  };
}
