{
  bash,
  coreutils,
  writeShellApplication,
  callPackage,
  ...
}:

let
  dnsResolverHelper = callPackage ./dns-resolver-helper.nix { };
in
# Script to fix GlobalProtect VPN DNS scoping on macOS.
# Creates /etc/resolver entries for VPN domains using privileged helper.
writeShellApplication {
  name = "dns-vpn-scoping-fix";
  runtimeInputs = [
    bash
    coreutils
    dnsResolverHelper
  ];
  text = builtins.readFile ../scripts/dns-vpn-scoping-fix;
}
