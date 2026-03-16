################################################################################
# Sytter is an IFTTT platform for a host.  Use it to do things such as ensure
# BlueTooth is disabled when the machine goes to sleep.
################################################################################
{ flake-inputs, pkgs, ... }:
let
  macos-keyboard-remap =
    pkgs.callPackage ../packages/macos-keyboard-remap.nix
      { };
in
{
  imports = [
    flake-inputs.sytter.darwinModules.default
  ];
  nixpkgs.overlays = [
    flake-inputs.sytter.overlays.default
  ];
  services.sytter = {
    enable = true;
    sytters = {
      keyboard-remap = {
        name = "macOS keyboard remap";
        description = "Apply key remapping when any device connects.";
        triggers = [
          {
            kind = "device-connection";
            events = [ "Add" ];
            device_types = [ "any" ];
          }
        ];
        executors = [
          {
            kind = "shell";
            script = "${macos-keyboard-remap}/bin/macos-keyboard-remap";
          }
        ];
      };
    };
  };
}
