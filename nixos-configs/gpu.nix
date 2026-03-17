{ pkgs, ... }:
{
  environment.systemPackages = [
    # Let us do a benchmark using OpenGL 2.0.  This can give us diagonstics but
    # also just generate some activity on the GPU for the purposes of testing
    # things like proc-siding to ensure GPU activity thresholds are met.
    pkgs.glmark2
  ];
}
