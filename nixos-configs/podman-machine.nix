################################################################################
# Enable Podman on this host.
################################################################################
{
  config,
  pkgs,
  stdenv,
  ...
}:
{
  imports = [
    ../nixos-modules/nix-darwin-podman.nix
  ];
  nix-darwin-custom.services.podman.enabled = true;
}
