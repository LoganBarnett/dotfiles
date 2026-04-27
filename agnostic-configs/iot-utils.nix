################################################################################
# CLI utilities for talking to local IoT devices (mini-splits, smart plugs,
# etc.) over the LAN, without going through vendor cloud services.
################################################################################
{ pkgs, ... }:
{
  environment.systemPackages = [
    (pkgs.callPackage ../derivations/gree-remote/default.nix { })
  ];
}
