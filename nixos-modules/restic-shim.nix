################################################################################
# Include my custom version of the Restic NixOS service.
################################################################################
{ pkgs, ... }: {
  disabledModules = [
    "services/backup/restic.nix"
  ];
  imports = [
    ./restic.nix
  ];
}
