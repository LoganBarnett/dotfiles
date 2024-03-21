# This module defines a small NixOS installation CD.  It does not
# contain any graphical stuff.

{ lib, ... }:

{
  imports = [
    ./minimal.nix
    ./installation-cd-base.nix
  ];

  # Causes a lot of uncached builds for a negligible decrease in size.
  environment.noXlibs = lib.mkOverride 500 false;

  # Documentation causes issues with cross compiling, and we don't generally
  # want this on servers anyways.
  documentation.man.enable = lib.mkOverride 500 false;
  documentation.doc.enable = lib.mkOverride 500 false;

  fonts.fontconfig.enable = lib.mkOverride 500 false;

  isoImage.edition = lib.mkOverride 500 "minimal";
}
