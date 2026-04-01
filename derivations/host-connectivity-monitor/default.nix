{ callPackage, writeShellApplication }:
let
  alert-me-locally = callPackage ../alert-me-locally/default.nix { };
  sonification-test = callPackage ../sonification-test/default.nix { };
in
writeShellApplication {
  name = "host-connectivity-monitor";
  runtimeInputs = [
    alert-me-locally
    sonification-test
  ];
  text = builtins.readFile ./host-connectivity-monitor;
}
