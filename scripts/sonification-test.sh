#!/usr/bin/env bash
set -euo pipefail

################################################################################
# Prototype script for exploring sonification sounds for server monitoring.
# Run it, tweak the sox parameters, and run it again.
#
# Usage: sonification-test.sh <sample> [style]
#
# Samples: ping, chime, thrum
# Styles:  normal, stressed, unhealthy, dead
#
# Only the normal style is currently implemented.  The design intent is that
# each style be a recognizable variant of the same "voice," so a listener can
# identify which service is distressed without consulting a dashboard.
################################################################################

command -v play >/dev/null 2>&1 || {
    echo "play (sox) not found; install sox or rebuild your Nix profile." >&2
    exit 1
}

sample="${1:-}"
style="${2:-normal}"

if [[ -z "${sample}" ]]; then
    echo "Usage: $(basename "$0") <sample> [style]" >&2
    echo "Samples: ping, chime, thrum" >&2
    echo "Styles:  normal, stressed, unhealthy, dead" >&2
    exit 1
fi

# A brief, clean rising chirp.  Lightweight feel, suitable for network-type
# or request-rate services.
play_normal_ping() {
    play -n synth 0.08 sine 880:1320 fade q 0.005 0.08 0.03 vol 0.35
}

# A two-tone descending bell.  More musical character; suitable for
# application-level services.
play_normal_chime() {
    play -n synth 0.2 sine 554 fade q 0.01 0.2 0.08 vol 0.3
    sleep 0.12
    play -n synth 0.15 sine 440 fade q 0.01 0.15 0.08 vol 0.25
}

# A slow-beating low tone.  The 2Hz tremolo modulation gives it a pulsing
# quality without any rhythm programming.  Suitable for storage or
# infrastructure services.
play_normal_thrum() {
    play -n synth 0.4 sine 111 tremolo 2 80 fade q 0.06 0.4 0.14 vol 0.45
}

case "${style}" in
    normal)
        case "${sample}" in
            ping)  play_normal_ping  ;;
            chime) play_normal_chime ;;
            thrum) play_normal_thrum ;;
            *)
                echo "Unknown sample: ${sample}" >&2
                echo "Available: ping, chime, thrum" >&2
                exit 1
                ;;
        esac
        ;;
    stressed|unhealthy|dead)
        echo "Style '${style}' is not yet implemented." >&2
        exit 1
        ;;
    *)
        echo "Unknown style: ${style}" >&2
        echo "Available: normal, stressed, unhealthy, dead" >&2
        exit 1
        ;;
esac
