{ ... }:
{
  services.hyuqueue-server = {
    enable = true;
    socket = null; # TCP mode — Emacs url.el needs HTTP, not unix socket
    # host defaults to "127.0.0.1" — localhost only
    # port defaults to 8731 — matches Emacs client default
  };
}
