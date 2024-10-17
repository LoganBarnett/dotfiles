final: prev: {
  # This issue manifested for me on macOS:
  # > link: golang.org/x/net/internal/socket: invalid reference to syscall.recvmsg
  # https://github.com/chipmk/docker-mac-net-connect/issues/39 lists a
  # workaround for adding the `-checklinkname=0` setting to LD_FLAGS.  That is
  # the only material change I could find.  I couldn't find any issues for this
  # in nixpkgs, so it's likely a less-used macOS only issue.  That checks out
  # for my understanding, because wireguard-go isn't typically used by Linux and
  # instead is for macOS and Windows.
  wireguard-go = prev.wireguard-go.overrideAttrs (old: {
    ldflags = old.ldflags ++ [
      "-checklinkname=0"
    ];
  });
}
