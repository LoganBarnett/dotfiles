{ bash
, gpclient
, coreutils
, writeShellApplication
, callPackage
, ...
}: let
  name = "gp-monitor";
  script = "${name}.sh";
  fixVpnDnsScoping = callPackage ./fix-vpn-dns-scoping.nix {};
in writeShellApplication {
  inherit name;
  runtimeInputs = [ bash gpclient coreutils fixVpnDnsScoping ];
  text = builtins.readFile ../scripts/${script};
}
