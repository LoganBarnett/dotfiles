{ facts, ... }:
{
  services.https.fqdns."ollama.${facts.network.domain}" = {
    enable = true;
    serviceNameForSocket = "garage-queue-server";
    socketGroup = "garage-queue";
  };

  # LLM inference can run for several minutes; raise nginx's default 60-second
  # proxy timeout so long-running generate requests are not cut short.
  # Workers connect via SSE, so disable proxy buffering to ensure events are
  # delivered immediately rather than being held in nginx's response buffer.
  services.nginx.virtualHosts."ollama.${facts.network.domain}".locations."/".extraConfig =
    ''
      proxy_read_timeout 10m;
      proxy_send_timeout 10m;
      proxy_buffering off;
    '';

  networking.dnsAliases = [ "ollama" ];
  networking.monitors = [ "garage-queue-server" ];
  services.garage-queue-server = {
    enable = true;
    queues.ollama.integrations.ollama.enable = true;
  };
}
