################################################################################
# Add graphics benchmarking programs.
#
# One can also get a quick idea with the WebGL aquarium:
# https://webglsamples.org/aquarium/aquarium.html
################################################################################
{ lib, pkgs, ... }: {
  allowUnfreePackagePredicates = [
    (pkg: builtins.elem (lib.getName pkg) [
      "Superposition"
      "unigine-valley"
      "unigine-heaven"
      "unigine-tropics"
      "unigine-sanctuary"
      "unigine-superposition"
    ])
  ];
  environment.systemPackages = [
    # pkgs.unigine-valley
    # pkgs.unigine-heaven
    # pkgs.unigine-tropics
    # pkgs.unigine-sanctuary
    # pkgs.unigine-superposition
    pkgs.phoronix-test-suite
    # I couldn't find these, but they are out there.
    # pkgs.glxgears
    # pkgs.vkcube
  ];
}
