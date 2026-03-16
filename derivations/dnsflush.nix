{ writeShellApplication }:
writeShellApplication {
  name = "dnsflush";
  # The script uses non-portable arithmetic syntax that shellcheck rejects.
  checkPhase = "";
  text = builtins.readFile ../scripts/dnsflush;
}
