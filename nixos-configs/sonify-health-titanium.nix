{
  config,
  facts,
  pkgs,
  ...
}:
{
  services.https.fqdns."titanium-sonify.${facts.network.domain}" = {
    serviceNameForSocket = "sonify-health";
  };

  services.sonify-health = {
    enable = true;
    logLevel = "debug";
    oidc = {
      enable = true;
      baseUrl = "https://titanium-sonify.${facts.network.domain}";
      issuer = "https://authelia.${facts.network.domain}";
      clientId = "titanium-sonify";
      clientSecretFile = config.age.secrets.titanium-sonify-oidc-client-secret.path;
    };
    # Use plughw: (direct hardware with format conversion) instead of
    # relying on ALSA hint enumeration, which routes through dmix.
    # dmix's MMAP mixing loop silently fails on this snd_hda_intel
    # device, resulting in appl_ptr stuck at 0 (silence).
    audioDevice = "plughw:CARD=0";
    patches = {
      reactor-ok = {
        freq = 350.0;
        overrides = "chirp";
      };
      reactor-error = {
        overrides = "chirp";
      };
      reactor-cpu-lo = {
        amplitude = 0.3;
        attack_ms = 0.0;
        brightness = 1.47;
        chirp_ratio = 1.0;
        crush = 0.08;
        decay_ms = 0.0;
        downsample = 0.03;
        drive = 2.01;
        duration = 1.95;
        echo_delay = 0.01;
        echo_mix = 0.18;
        fm_depth = 0.7;
        fm_ratio = 0.04;
        freq = 15.0;
        gap = 0.0;
        highpass = 0.0;
        noise_mix = 1.0;
        release_ms = 0.0;
        resonance = 3.78;
        reverb_mix = 0.0;
        saw_ratio = 0.0;
        sine_ratio = 0.0;
        square_ratio = 1.0;
        stereo_pan = 0.0;
        sub_octave = 0.26;
        sustain = 1.0;
        tremolo_depth = 0.55;
        tremolo_rate = 5.0;
        tri_ratio = 0.0;
        vibrato_depth = 0.0;
        vibrato_rate = 0.0;
      };
      reactor-cpu-hi = {
        freq = 18.0;
        overrides = "reactor-cpu-lo";
      };
    };
    heartbeats = [
      {
        name = "internal";
        command = "${pkgs.fping}/bin/fping -q -t 4000 -r 1 192.168.254.254 192.168.254.9 silicon.proton";
        resultMode = "exit-code";
        cycleSecs = 15.0;
        playback = "clock";
        cycleOffsetSecs = 9.0;
        notes = [
          {
            transition = {
              type = "discrete";
              states = [
                {
                  threshold = 0.5;
                  patch = "reactor-ok";
                }
                {
                  threshold = 1.01;
                  patch = "reactor-error";
                }
              ];
            };
          }
        ];
      }
      {
        name = "external";
        command = "${pkgs.fping}/bin/fping -q -t 4000 -r 1 208.67.222.222 9.9.9.9 resolver1.opendns.com api.anthropic.com";
        resultMode = "exit-code";
        cycleSecs = 15.0;
        playback = "clock";
        cycleOffsetSecs = 10.0;
        notes = [
          {
            transition = {
              type = "discrete";
              states = [
                {
                  threshold = 0.5;
                  patch = "reactor-ok";
                }
                {
                  threshold = 1.01;
                  patch = "reactor-error";
                }
              ];
            };
          }
          {
            offset = 0.15;
            transition = {
              type = "discrete";
              states = [
                {
                  threshold = 0.5;
                  patch = "reactor-ok";
                }
                {
                  threshold = 1.1;
                  patch = "reactor-error";
                }
              ];
            };
          }
        ];
      }
      {
        name = "gpu";
        command = "${pkgs.gawk}/bin/awk 'BEGIN{getline v < \"${config.hardware.amdGpuCard.sysfsPath}/gpu_busy_percent\"; printf \"%.2f\\n\", v/100}'";
        resultMode = "stdout";
        cycleSecs = 15.0;
        playback = "continuous";
        cycleOffsetSecs = 0.0;
        pollIntervalSecs = 5.0;
        notes = [
          {
            volume = 0.55;
            transition = {
              type = "gradient";
              patches = [
                "reactor-cpu-lo"
                "reactor-cpu-hi"
              ];
              segments = [
                {
                  strategy = "ease-in";
                  intensity = 2.0;
                }
              ];
            };
          }
        ];
      }
    ];
  };
}
