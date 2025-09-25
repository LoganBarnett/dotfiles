################################################################################
# Manage software and configuration for a typical workstation.
################################################################################
{ pkgs, ... }: {

  imports = [
    ../agnostic-configs/btop.nix
  ];

}
