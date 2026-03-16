################################################################################
# Goss health check for Ollama Metal GPU acceleration on darwin.
#
# Verifies that Ollama is using Metal (GPU) by timing a small inference.  If
# no model is currently loaded, Ollama is idle and the check passes trivially.
#
# Background: on Apple Silicon, memory is unified — size_vram always equals
# size when a model is loaded regardless of whether Metal or CPU is doing the
# compute.  vram metrics cannot distinguish GPU from CPU inference on this
# platform, so a latency probe is the only reliable approach.
#
# The 5-second (5000 ms) threshold is intentionally generous: Metal completes
# a 5-token response in well under 1 second.  CPU fallback (which Ollama uses
# when Metal initialisation fails) is orders of magnitude slower and
# consistently exceeds the threshold.  Millisecond precision is used because
# Nix provides GNU coreutils date, which supports date +%s%3N.
################################################################################
{ pkgs, ... }:
{
  services.goss.checks = {
    command."ollama-metal-acceleration" = {
      exec = ''
        models=$(${pkgs.curl}/bin/curl -sf http://localhost:11434/api/ps) || exit 1
        count=$(printf '%s' "$models" | ${pkgs.jq}/bin/jq '.models | length')
        [ "$count" -eq 0 ] && exit 0

        model=$(printf '%s' "$models" | ${pkgs.jq}/bin/jq -r '.models[0].name')

        start_ms=$(date +%s%3N)
        ${pkgs.curl}/bin/curl -sf \
          -X POST http://localhost:11434/api/generate \
          -H 'Content-Type: application/json' \
          --data "{\"model\":\"$model\",\"prompt\":\"Hi\",\"stream\":false,\"options\":{\"num_predict\":5}}" \
          > /dev/null || exit 1
        elapsed_ms=$(( $(date +%s%3N) - start_ms ))

        [ "$elapsed_ms" -lt 5000 ]
      '';
      "exit-status" = 0;
      timeout = 30000;
    };
  };
}
