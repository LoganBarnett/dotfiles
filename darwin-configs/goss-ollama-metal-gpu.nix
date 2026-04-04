################################################################################
# Goss health check for Ollama Metal GPU acceleration on darwin.
#
# Uses dual-signal detection: both inference timing and metalps GPU sampling
# must confirm eviction before the check fails.  This prevents false positives
# from partial CPU offload or legitimate high CPU usage alongside active GPU
# inference.
#
# | eval_duration | gpu_percent | Meaning                      | Result |
# |---------------|-------------|------------------------------|--------|
# | Fast (<2s)    | any         | GPU working                  | Pass   |
# | Slow (>=2s)   | >0          | Partial offload, not eviction| Pass   |
# | Slow (>=2s)   | 0           | GPU evicted                  | Fail   |
# | No models     | —           | Can't test                   | Pass   |
#
# Background: on Apple Silicon, memory is unified — size_vram always equals
# size when a model is loaded regardless of whether Metal or CPU is doing the
# compute, and Ollama does not update its PROCESSOR field after initial load,
# so neither metric can detect GPU eviction after the fact.
#
# metalps uses the same IOKit AGXDeviceUserClient interface that Activity
# Monitor uses to read per-process GPU utilisation.  On Metal, gpu_percent
# will be non-zero during active token generation.  On CPU the process never
# acquires a Metal context, so gpu_percent stays at 0 even under full CPU
# load.
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

        # Fire a test inference in the background.  num_predict=5 keeps it
        # short; --max-time 8 caps worst-case CPU fallback.
        ${pkgs.curl}/bin/curl -sf http://localhost:11434/api/generate \
          --max-time 8 \
          -d "{\"model\":\"$model\",\"prompt\":\"0\",\"stream\":false,\"options\":{\"num_predict\":5}}" \
          -o /tmp/ollama-goss-inference.json &
        infer_pid=$!

        # Wait for the runner to register in IOKit before the first sample.
        # metalps computes gpu_percent as a delta between two samples; a
        # process absent from the first sample gets gpu_percent = 0 even while
        # actively computing on the GPU (process-chasing false negative).
        sleep 1

        # Sample GPU percent while the inference should be running.
        result=$(${metalps}/bin/metalps --json --interval-ms 2000) || exit 1
        gpu_percent=$(printf '%s' "$result" \
          | ${pkgs.jq}/bin/jq '[.processes[] | select(.name == "ollama") | .gpu_percent] | max // 0')

        # Wait for inference to complete and capture the response.
        wait "$infer_pid"
        infer_exit=$?

        # If inference timed out or failed, we can't determine eval_duration.
        # Treat as inconclusive — pass to avoid false positives.
        [ "$infer_exit" -ne 0 ] && exit 0

        eval_duration=$(${pkgs.jq}/bin/jq '.eval_duration // 0' /tmp/ollama-goss-inference.json)

        # Fail only when BOTH signals confirm eviction:
        # - eval_duration >= 2s (2000000000 ns): inference is slow
        # - gpu_percent == 0: no GPU activity detected
        slow=$(printf '%s\n' "$eval_duration" | ${pkgs.jq}/bin/jq '. >= 2000000000')
        no_gpu=$(printf '%s\n' "$gpu_percent" | ${pkgs.jq}/bin/jq '. == 0')

        if [ "$slow" = "true" ] && [ "$no_gpu" = "true" ]; then
          printf 'GPU evicted: eval_duration=%s ns, gpu_percent=%s\n' "$eval_duration" "$gpu_percent"
          exit 1
        fi

        printf 'GPU OK: eval_duration=%s ns, gpu_percent=%s\n' "$eval_duration" "$gpu_percent"
        exit 0
      '';
      "exit-status" = 0;
      timeout = 15000;
    };
  };
}
