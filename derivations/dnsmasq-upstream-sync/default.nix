{ coreutils, writeShellApplication }:
writeShellApplication {
  name = "dnsmasq-upstream-sync";
  runtimeInputs = [ coreutils ];
  text = builtins.readFile ./dnsmasq-upstream-sync;
}
