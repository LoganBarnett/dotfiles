{
  bash,
  gpclient,
  coreutils,
  writeShellApplication,
  callPackage,
  ...
}:
let
  name = "gp-monitor";
  script = name;
  dnsVpnScopingFix = callPackage ./dns-vpn-scoping-fix.nix { };
in
writeShellApplication {
  inherit name;
  runtimeInputs = [
    bash
    gpclient
    coreutils
    dnsVpnScopingFix
  ];
  text = builtins.readFile ../scripts/${script};
}
