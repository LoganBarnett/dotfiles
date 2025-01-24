################################################################################
# Include my custom version of the Octoprint NixOS service.
################################################################################
{
  disabledModules = [
    "services/misc/octoprint.nix"
  ];
  imports = [
    ./octoprint.nix
  ];
}
