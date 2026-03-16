{ writeShellApplication }:
writeShellApplication {
  name = "vpn-dns-recover";
  # The script sources ~/.bash-logging at runtime, which Home Manager manages.
  # It also uses deprecated egrep.  Shellcheck cannot follow the dynamic
  # source path.
  checkPhase = "";
  text = builtins.readFile ../scripts/vpn-dns-recover;
}
