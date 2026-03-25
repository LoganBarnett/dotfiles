{ config, facts, ... }:
let
  # Estimate VRAM requirement in MB from the model name.  Parses the
  # parameter count (e.g. "8b" → 8) and quantisation level (e.g. "q4" →
  # 600 MB/B, defaulting to q4 if absent), then adds a fixed overhead.
  vramJq = builtins.toFile "vram.jq" ''
    .model as $m |
    ({"q4": 600, "q5": 700, "q8": 1000, "f16": 2000}) as $mult |
    ({"mixtral:8x7b": 26000}) as $overrides |

    $overrides[$m] //
    (
      ($m | capture("(?<n>[0-9]+\\.?[0-9]*)b").n | tonumber) as $params |
      ($m | (match("(q[0-9]+|f[0-9]+)").captures[0].string) // "q4") as $quant |
      ($mult[$quant] // 600) as $mbPerB |
      ($params * $mbPerB + 1500)
    )
  '';
in
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

  services.garage-queue-server = {
    enable = true;
    nats.enable = true;
    settings = {
      server = {
        host = "0.0.0.0";
        port = 9090;
        nats_url = "nats://127.0.0.1:4222";
        generate_queue = "ollama";
      };
      queues.ollama = {
        route = "/api/generate";
        extractors = {
          model_tag = {
            kind = "tag";
            capability = "model";
            jq_exp = ".model";
          };
          vram = {
            kind = "scalar";
            capability = "vram_mb";
            jq_file = "${vramJq}";
          };
        };
      };
    };
  };
}
