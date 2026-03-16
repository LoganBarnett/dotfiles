{ writeShellApplication }:
writeShellApplication {
  name = "host-wait";
  # Note: the script uses GNU ping syntax (--count, --quiet), which differs
  # from the BSD ping available on macOS.  A future fix would adapt to the
  # platform or substitute pkgs.iputils, which only targets Linux.
  text = builtins.readFile ../scripts/host-wait;
}
