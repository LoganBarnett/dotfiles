################################################################################
# Goss health check for Ollama Metal GPU acceleration on darwin.
#
# Verifies that Ollama is using Metal (GPU) by inspecting its accumulated GPU
# time via metalps.  If no model is currently loaded, the check passes
# trivially.
#
# Background: on Apple Silicon, memory is unified — size_vram always equals
# size when a model is loaded regardless of whether Metal or CPU is doing the
# compute, and Ollama does not update its PROCESSOR field after initial load,
# so neither metric can detect GPU eviction after the fact.
#
# metalps uses the same IOKit AGXDeviceUserClient interface that Activity
# Monitor uses to read per-process accumulated GPU time (gpu_time_ns).  When
# Ollama is loaded on Metal, this counter is non-zero.  When Ollama falls back
# to CPU (eviction, Metal initialisation failure, etc.) it never acquires a GPU
# client connection and the counter stays at zero — metalps omits the process
# from its output entirely, giving us an empty processes array.
################################################################################
{
  flake-inputs,
  pkgs,
  system,
  ...
}:
let
  metalps = flake-inputs.metalps.packages.${system}.cli;
in
{
  services.goss.checks = {
    command."ollama-metal-acceleration" = {
      exec = ''
        models=$(${pkgs.curl}/bin/curl -sf http://localhost:11434/api/ps) || exit 1
        count=$(printf '%s' "$models" | ${pkgs.jq}/bin/jq '.models | length')
        [ "$count" -eq 0 ] && exit 0

        # Sample all GPU-active processes.  Do not filter by PID: Ollama spawns
        # a runner subprocess (ollama runner --model ...) that holds the actual
        # Metal context.  The main server process (ollama serve) has no GPU time
        # and would always produce a false failure if targeted directly.
        result=$(${metalps}/bin/metalps --json --interval-ms 500) || exit 1
        gpu_time=$(printf '%s' "$result" \
          | ${pkgs.jq}/bin/jq '[.processes[] | select(.name == "ollama") | .gpu_time_ns] | max // 0')

        [ "$gpu_time" -gt 0 ]
      '';
      "exit-status" = 0;
      timeout = 10000;
    };
  };
}
