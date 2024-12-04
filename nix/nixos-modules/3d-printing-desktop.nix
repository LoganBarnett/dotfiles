################################################################################
# Include tools for 3D printing on one's desktop.
################################################################################
{ lib, pkgs, ... }: {
  allowUnfreePackagePredicates = [
    (pkg: builtins.elem (lib.getName pkg) [
      "blender"
    ])
  ];
  environment.systemPackages = [
    pkgs.blender
    pkgs.openscad
    pkgs.prusa-slicer
  ];
}
