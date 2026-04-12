################################################################################
# Generic audio tools available on all Linux hosts.
################################################################################
{ pkgs, ... }:
{
  environment.systemPackages = [
    # Provides speaker-test, aplay, arecord, amixer, etc. for audio
    # diagnostics and troubleshooting.
    pkgs.alsa-utils
  ];
}
