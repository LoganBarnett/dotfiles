{ facts, ... }:
{
  services.https.fqdns."ollama.${facts.network.domain}" = {
    enable = true;
    serviceNameForSocket = "garage-queue-server";
    socketGroup = "garage-queue";
  };

  # LLM inference can run for several minutes; raise nginx's default 60-second
  # proxy timeout so long-running generate requests are not cut short.
  services.nginx.virtualHosts."ollama.${facts.network.domain}".locations."/".extraConfig =
    ''
      proxy_read_timeout 10m;
      proxy_send_timeout 10m;
    '';

  networking.dnsAliases = [ "ollama" ];
  services.garage-queue-server = {
    enable = true;
    nats.enable = true;
    queues.ollama.integrations.ollama.enable = true;
    settings.server = {
      # host and port are managed by the Nix module's top-level options
      # (socket mode by default).  Only nats_url needs to be here.
      nats_url = "nats://127.0.0.1:4222";
    };
  };
}
