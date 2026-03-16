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
    # openusd is failing, but it looks like it's fixed in
    # https://github.com/NixOS/nixpkgs/pull/380449
    # which I do not have in my nixpkgs yet.
    # pkgs.blender
    # pkgs.openscad
    # pkgs.prusa-slicer
  ];
}
