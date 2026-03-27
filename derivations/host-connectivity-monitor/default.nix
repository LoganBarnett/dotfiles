{ callPackage, writeShellApplication }:
let
  alert-me-locally = callPackage ../alert-me-locally/default.nix { };
in
writeShellApplication {
  name = "host-connectivity-monitor";
  runtimeInputs = [ alert-me-locally ];
  text = builtins.readFile ./host-connectivity-monitor;
}
