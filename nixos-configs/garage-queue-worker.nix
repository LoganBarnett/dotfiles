{
  facts,
  host-id,
  lib,
  ...
}:
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

  services.https.fqdns."${host-id}-ollama-worker.${facts.network.domain}" = {
    socket = "/run/garage-queue-worker-ollama/observe.sock";
  };

  # nginx needs the garage-queue group to traverse /run/garage-queue-worker-*
  # and connect to the observe socket.
  users.users.nginx.extraGroups = [ "garage-queue" ];

  # Worker must create sockets with group-write so nginx can connect.
  systemd.services.garage-queue-worker-ollama.serviceConfig.UMask =
    lib.mkDefault "0007";

  networking.dnsAliases = [ "${host-id}-ollama-worker" ];
  networking.monitors = [ "garage-queue-worker" ];
}
