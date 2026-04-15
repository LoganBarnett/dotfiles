################################################################################
# A series of tools for helping manage the Nix store.
#
# Typically we want everything declarative but sometimes the store gets into a
# wonky state and we need tools to help us un-wonky it.
################################################################################
{ pkgs, ... }:
{
  environment.systemPackages = [
    (pkgs.callPackage ../derivations/nix-store-diagnose.nix { })
    (pkgs.callPackage ../derivations/nix-store-dislodge.nix { })
  ];
}
