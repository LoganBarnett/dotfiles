################################################################################
# Webhook endpoint for Ollama Metal GPU remediation.
#
# Ollama does not detect when macOS evicts it from the GPU.  It continues
# reporting 100% GPU via /api/ps while silently falling back to CPU inference,
# which is ~450x slower for this workload.
#
# When the goss check (darwin-configs/goss-ollama-metal-gpu.nix) detects zero
# accumulated GPU time for the Ollama process, it surfaces a failure through
# Prometheus.  Alertmanager then POSTs to this webhook endpoint, which restarts
# the Ollama user agent so macOS can re-initialise the Metal context and
# recover GPU acceleration.
################################################################################
{
  config,
  lib-custom,
  pkgs,
  ...
}:
let
  # Ollama runs as a launchd user agent under the primary user.  Kicking it
  # with -k (kill before restart) causes launchd to tear down the Metal
  # context fully before bringing it back up, which is what forces GPU
  # re-acquisition.
  ollamaRestartScript = pkgs.writeShellScript "ollama-metal-restart" ''
    uid=$(id -u ${config.system.primaryUser})
    launchctl asuser "$uid" launchctl kickstart -k \
      "gui/$uid/org.nixos.ollama"
  '';
in
{
  imports = [ ../darwin-modules/webhook.nix ];

  age.secrets.ollama-webhook-token = {
    generator.script = "base64";
  };

  services.webhook = {
    enable = true;
    # Alertmanager reaches this from another host on the proton network.
    openFirewall = true;
    secretEnvVars.OLLAMA_WEBHOOK_TOKEN =
      config.age.secrets.ollama-webhook-token.path;
    hooksTemplated.ollama-restart = lib-custom.mkAuthedHook {
      id = "ollama-restart";
      tokenEnvVar = "OLLAMA_WEBHOOK_TOKEN";
      command = "${ollamaRestartScript}";
    };
  };
}
