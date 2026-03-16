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
# Monitor uses to read per-process GPU utilisation.  The check fires a minimal
# test inference so the runner subprocess is actively computing during the
# sample window, then inspects gpu_percent.  On Metal, gpu_percent will be
# non-zero during active token generation.  On CPU the process never acquires a
# Metal context, so gpu_percent stays at 0 even under full CPU load.
#
# Checking gpu_time_ns (cumulative) is insufficient: a long-lived process that
# previously held a GPU context retains a non-zero counter even after eviction,
# producing a false pass.
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

        model=$(printf '%s' "$models" | ${pkgs.jq}/bin/jq -r '.models[0].name')

        # Fire a test inference in the background so the runner subprocess is
        # actively computing during the metalps sample window.  num_predict must
        # be large enough that generation still running at t=1s (after the
        # sleep below): a fast 3B model on M1 Max produces ~80 t/s, so 200
        # tokens ≈ 2.5 s of GPU compute, well past the sampling window.
        # --max-time caps the request in CPU fallback (2 t/s × 200 = 100 s
        # uncapped) so the check never blocks for more than 6 s.
        ${pkgs.curl}/bin/curl -sf http://localhost:11434/api/generate \
          --max-time 6 \
          -d "{\"model\":\"$model\",\"prompt\":\"0\",\"stream\":false,\"options\":{\"num_predict\":200}}" \
          -o /dev/null &

        # Wait for the runner to register in IOKit before the first sample.
        # metalps computes gpu_percent as a delta between two samples; a
        # process absent from the first sample gets gpu_percent = 0 even while
        # actively computing on the GPU (process-chasing false negative).
        # Timing: ~0s (api/ps) + 1s (sleep) + 2s (metalps) + ≤3s (wait) < 10s.
        sleep 1

        # Sample GPU percent while the inference should be running.  On Metal
        # the runner holds a live GPU context and gpu_percent will be non-zero
        # during token generation.  On CPU the process never acquires a Metal
        # context so gpu_percent stays at 0 regardless of CPU load.
        result=$(${metalps}/bin/metalps --json --interval-ms 2000) || exit 1
        wait

        gpu_percent=$(printf '%s' "$result" \
          | ${pkgs.jq}/bin/jq '[.processes[] | select(.name == "ollama") | .gpu_percent] | max // 0')

        # jq -e exits 1 when the expression is false, which is what goss checks.
        printf '%s\n' "$gpu_percent" | ${pkgs.jq}/bin/jq -e '. > 0'
      '';
      "exit-status" = 0;
      timeout = 10000;
    };
  };
}
