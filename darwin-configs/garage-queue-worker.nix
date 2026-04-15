{ facts, host-id, ... }:
{
  services.garage-queue-worker.workers.ollama = {
    enable = true;
    integrations.ollama.enable = true;
    settings = {
      worker = {
        id = host-id;
        server_url = "https://ollama.${facts.network.domain}";
        poll_interval_ms = 1000;
      };
    };
  };
}
