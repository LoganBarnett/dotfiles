# Global system administration tools.
{ pkgs, ... }: {
  environment.systemPackages = [
    # A grep-sed like alternative. Offers a scripting language for
    # transformations.
    pkgs.ack
    # This is a suite of GNU utilities.
    pkgs.coreutils
    # curl does http requests. Comes with MacOS but no reason to use a dated
    # version.
    pkgs.curl
    # Allows us to query the status of USB devices.  This uses lsusb or
    # systemprofile -json under the hood in a cross-platform manner.
    # Unfortunately it does not work on non-USB devices (like SD cards) like one
    # might think.  This is _not_ for storage devices (many things imply it will
    # work, but it won't).
    pkgs.cyme
    # Searches for files. Used by projectile in Emacs.  Included in
    # administrative tools because it needs to be on the remote host when using
    # Tramp.
    pkgs.fd
    # Who wants BSD grep when you could have GNU grep?
    pkgs.gnugrep
    # Who wants BSD sed when you could have GNU sed?
    pkgs.gnused
    # Who wants BSD ls when you could have GNU ls? And other GNU things.
    pkgs.gnutls
    # A more visual version of top.
    pkgs.htop
    # A union of ping and traceroute - ping all hosts along a route.
    pkgs.mtr
    # Use Unix pipes and direct them to a human program.
    pkgs.percol
    # Show progress via a pipe, such as with dd.
    pkgs.pv
    # An n-curses based interface to du.  Shows disk usage in a way that makes
    # identification/cleaning quick+easy.  See: https://dev.yorhel.nl/ncdu
    pkgs.ncdu
    # Run speed tests from the command line.
    pkgs.speedtest-cli
    # A self-proclaimed better netcat.
    pkgs.socat
    # Really fast grep alternative.
    pkgs.ripgrep
    # Copy files recursively. Replaces BSD version on macOS.
    pkgs.rsync
    # Show a tree-listing of directories and files.
    pkgs.tree
    # Highly controllable terminal emulation and session management.
    pkgs.tmux
    # The penultimate editor.
    pkgs.vim
    # A handy alternative to curl, best suited for downloading content.
    pkgs.wget
  ];
}
