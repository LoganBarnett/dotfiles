################################################################################
# Goss health checks for Ollama Metal GPU usage on darwin.
#
# Detects if Ollama has been pushed off the GPU under memory pressure.  If no
# models are currently loaded Ollama is idle and both checks pass trivially.
#
# Two distinct failure states are separated to aid debugging:
#
#   ollama-full-gpu-eviction   — a loaded model has size_vram == 0; it was
#                                pushed entirely onto CPU.
#   ollama-partial-gpu-eviction — a loaded model has 0 < size_vram < size;
#                                part of it was paged out to CPU while the
#                                rest remains on GPU.
################################################################################
{ pkgs, ... }:
{
  services.goss.checks = {
    command."ollama-full-gpu-eviction" = {
      exec = ''
        models=$(${pkgs.curl}/bin/curl -sf http://localhost:11434/api/ps) || exit 1
        count=$(printf '%s' "$models" | ${pkgs.jq}/bin/jq '.models | length')
        [ "$count" -eq 0 ] && exit 0
        fully_evicted=$(printf '%s' "$models" | ${pkgs.jq}/bin/jq '[.models[] | select(.size_vram == 0)] | length')
        [ "$fully_evicted" -eq 0 ]
      '';
      "exit-status" = 0;
      timeout = 5000;
    };
    command."ollama-partial-gpu-eviction" = {
      exec = ''
        models=$(${pkgs.curl}/bin/curl -sf http://localhost:11434/api/ps) || exit 1
        count=$(printf '%s' "$models" | ${pkgs.jq}/bin/jq '.models | length')
        [ "$count" -eq 0 ] && exit 0
        partially_evicted=$(printf '%s' "$models" | ${pkgs.jq}/bin/jq '[.models[] | select(.size_vram > 0 and .size_vram < .size)] | length')
        [ "$partially_evicted" -eq 0 ]
      '';
      "exit-status" = 0;
      timeout = 5000;
    };
  };
}
