{
  config,
  facts,
  pkgs,
  ...
}:
{
  services.https.fqdns."silicon-sonify.${facts.network.domain}" = {
    serviceNameForSocket = "sonify-health";
  };
  environment.systemPackages = [ pkgs.alsa-utils ];

  systemd.services.alsa-unmute = {
    description = "Unmute ALSA master volume";
    wantedBy = [ "sonify-health.service" ];
    before = [ "sonify-health.service" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${pkgs.alsa-utils}/bin/amixer -D hw:0 sset Master 100% unmute";
    };
  };

  services.sonify-health = {
    enable = true;
    logLevel = "debug";
    oidc = {
      enable = true;
      baseUrl = "https://silicon-sonify.${facts.network.domain}";
      issuer = "https://authelia.${facts.network.domain}";
      clientId = "silicon-sonify";
      clientSecretFile = config.age.secrets.silicon-sonify-oidc-client-secret.path;
    };
    patches = {
      medbay-ok = {
        amplitude = 0.327;
        attack_ms = 6.0;
        brightness = 1.82;
        chirp_ratio = 1.01;
        crush = 0.0;
        decay_ms = 97.0;
        detune = 0.0;
        downsample = 0.0;
        drive = 0.5;
        duration = 0.22;
        echo_delay = 0.32;
        echo_mix = 0.33;
        fm_depth = 0.0;
        fm_ratio = 0.0;
        freq = 182.0;
        gap = 0.0;
        harshness_offset = 0.0;
        highpass = 0.0;
        noise_mix = 0.0;
        release_ms = 22.0;
        resonance = 3.72;
        reverb_mix = 0.89;
        saw_ratio = 0.02;
        sine_ratio = 2.37;
        square_ratio = 0.0;
        stereo_pan = -0.42;
        sub_octave = 0.03;
        sustain = 1.0;
        tremolo_depth = 0.11;
        tremolo_rate = 0.0;
        tri_ratio = 1.22;
        vibrato_depth = 0.49;
        vibrato_rate = 0.0;
      };
      medbay-error = {
        amplitude = 0.54;
        chirp_ratio = 1.01;
        detune = 92.0;
        harshness_offset = 0.12;
        overrides = "medbay-ok";
      };
      medbay-cpu-lo = {
        amplitude = 0.3;
        attack_ms = 2000.0;
        brightness = 0.54;
        chirp_ratio = 1.0;
        crush = 0.0;
        decay_ms = 2000.0;
        detune = 0.0;
        downsample = 0.03;
        drive = 0.01;
        duration = 4.17;
        echo_delay = 0.01;
        echo_mix = 0.18;
        fm_depth = 0.7;
        fm_ratio = 0.0;
        freq = 55.0;
        gap = 0.0;
        harshness_offset = 0.0;
        highpass = 0.0;
        noise_mix = 1.0;
        release_ms = 100.0;
        resonance = 3.87;
        reverb_mix = 0.0;
        saw_ratio = 0.0;
        sine_ratio = 1.32;
        square_ratio = 1.0;
        stereo_pan = 0.0;
        sub_octave = 0.0;
        sustain = 0.0;
        tremolo_depth = 0.0;
        tremolo_rate = 0.0;
        tri_ratio = 0.0;
        vibrato_depth = 0.0;
        vibrato_rate = 0.0;
      };
      medbay-cpu-hi = {
        freq = 182.0;
        resonance = 1.8;
        brightness = 0.3;
        overrides = "medbay-cpu-lo";
      };
    };
    heartbeats = [
      {
        name = "internal";
        command = "${pkgs.fping}/bin/fping -q -t 4000 -r 1 192.168.254.254 192.168.254.6 titanium.proton";
        resultMode = "exit-code";
        cycleSecs = 15.0;
        playback = "clock";
        cycleOffsetSecs = 12.0;
        notes = [
          {
            transition = {
              type = "discrete";
              states = [
                {
                  threshold = 0.5;
                  patch = "medbay-ok";
                }
                {
                  threshold = 1.01;
                  patch = "medbay-error";
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
        cycleOffsetSecs = 13.0;
        notes = [
          {
            transition = {
              type = "discrete";
              states = [
                {
                  threshold = 0.5;
                  patch = "medbay-ok";
                }
                {
                  threshold = 1.01;
                  patch = "medbay-error";
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
                  patch = "medbay-ok";
                }
                {
                  threshold = 1.1;
                  patch = "medbay-error";
                }
              ];
            };
          }
        ];
      }
      {
        name = "cpu";
        command = "${pkgs.gawk}/bin/awk '{nproc='\"$(${pkgs.coreutils}/bin/nproc)\"'; printf \"%.2f\\n\", $1/nproc}' /proc/loadavg";
        resultMode = "stdout";
        cycleSecs = 15.0;
        playback = "loop";
        cycleOffsetSecs = 0.0;
        pollIntervalSecs = 5.0;
        phraseGap = 7.0;
        notes = [
          {
            volume = 0.55;
            transition = {
              type = "gradient";
              patches = [
                "medbay-cpu-lo"
                "medbay-cpu-hi"
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
