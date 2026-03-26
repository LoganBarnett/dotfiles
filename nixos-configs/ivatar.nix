################################################################################
# ivatar Libravatar-compatible avatar service on silicon.
################################################################################
{ ... }:
{
  imports = [ ../nixos-modules/ivatar.nix ];

  services.ivatar-host.enable = true;
}
