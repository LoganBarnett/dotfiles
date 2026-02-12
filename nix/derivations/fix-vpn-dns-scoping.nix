{ bash, coreutils, writeShellApplication, ... }:

writeShellApplication {
  name = "fix-vpn-dns-scoping";
  runtimeInputs = [ bash coreutils ];
  text = builtins.readFile ../scripts/fix-vpn-dns-scoping.sh;
}
