# Global system administration tools.
{ flake-inputs, system }: { lib, pkgs, ... }: {
  environment.systemPackages = [
    # A grep-sed like alternative. Offers a scripting language for
    # transformations.
    pkgs.ack
    # This is a suite of GNU utilities.
    pkgs.coreutils
    # curl does http requests. Comes with MacOS but no reason to use a dated
    # version.
    pkgs.curl
    # Show us the current system as a double.
    flake-inputs.current-system.packages.${system}.default
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
    # Perhaps the everlasting version control system.
    # git is needed on a server to do various Nix flake operations.
    pkgs.git
    # Who wants BSD grep when you could have GNU grep?
    pkgs.gnugrep
    # Who wants BSD sed when you could have GNU sed?
    pkgs.gnused
    # Who wants BSD ls when you could have GNU ls? And other GNU things.
    pkgs.gnutls
    # A more visual version of top.
    pkgs.htop
    # Performance test IP connections.  Supports TCP/UDP/SCTP and both IPv4 and
    # IPv6.  It requires running a client and a server simultaneously.
    pkgs.iperf
    # Let us see ports and file handles held by processes.
    pkgs.lsof
    # A union of ping and traceroute - ping all hosts along a route.
    pkgs.mtr
    # Use Unix pipes and direct them to a human program.
    # Sometimes pypi just fails to look up the requisite packages.
    # pkgs.percol
    # Show progress via a pipe, such as with dd.
    pkgs.pv
    # An n-curses based interface to du.  Shows disk usage in a way that makes
    # identification/cleaning quick+easy.  See: https://dev.yorhel.nl/ncdu
    # Disabled due to: https://github.com/NixOS/nixpkgs/issues/317055
    # The workaround can be found in ../overlays/zig.nix but it isn't working.
    pkgs.ncdu
    # Run speed tests from the command line.
    pkgs.speedtest-cli
    # A self-proclaimed better netcat.
    pkgs.socat
    # Really fast grep alternative.  Required by Emacs (which is also used via
    # Tramp).
    pkgs.ripgrep
    # Copy files recursively. Replaces BSD version on macOS.
    pkgs.rsync
    # Watch TCP packets!
    pkgs.tcpdump
    # Show a tree-listing of directories and files.
    pkgs.tree
    # Highly controllable terminal emulation and session management.  Have
    # persistent sessions (in case of SSH disconnection) among other things.
    pkgs.tmux
    # Disk health monitoring and reporting tools.  Provides smartctl and smartd.
    pkgs.smartmontools
    # We need an editor.  Even if we don't want to edit configuration files
    # because we already have Nix, it's still helpful for things that use
    # EDTIOR, such as C-x C-e in the shell.  Can we just uninstall nano?
    pkgs.vim
    # A handy alternative to curl, best suited for downloading content.
    pkgs.wget
  ] ++ lib.optionals pkgs.stdenv.isLinux [
    # Use for debugging network issues such as checking full or half duplex.
    pkgs.ethtool
    # Partition editing.
    pkgs.parted
    # Gives us iostat.  Show us IO usage (such as disk reads/writes).  Useful
    # for identifying performance bottlenecks relating to disk.
    pkgs.sysstat
    # Show the path that packets take.
    pkgs.traceroute
  ];
}
