################################################################################
# Include my custom version of the Octoprint NixOS service.
################################################################################
{ pkgs, ... }: {
  disabledModules = [
    "services/misc/octoprint.nix"
  ];
  imports = [
    # (pkgs.callPackage ./octoprint.nix {})
    ./octoprint.nix
  ];
}
