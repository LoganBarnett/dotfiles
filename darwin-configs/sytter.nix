################################################################################
# Sytter is an IFTTT platform for a host.  Use it to do things such as ensure
# BlueTooth is disabled when the machine goes to sleep.
################################################################################
{ flake-inputs, ... }: {
  imports = [
    flake-inputs.sytter.darwinModules.default
  ];
  nixpkgs.overlays = [
    flake-inputs.sytter.overlays.default
  ];
  services.sytter = {
    enable = true;
  };
}
