# -*- mode: Nix; dtrt-indent-mode: 0; tab-width: 2; standard-indent: 2; -*-
# The Emacs file variable line above is requied because dtrt-indent-mode
# Global system administration tools.
{ config, flake-inputs, lib, pkgs, system, ... }: {
  environment.variables = {
    # Why this defaults to nano is beyond me.
    EDITOR = "vim";
  };
  environment.systemPackages = [
    # A functional lens of updating configuration values for virtually any
    # kind.  You probably want to run `augtool` but there are other `aug*`
    # executables available.  There is no `augeas` executable.
    pkgs.augeas
    # A grep-sed like alternative. Offers a scripting language for
    # transformations.
    pkgs.ack
    # This is a suite of GNU utilities.
    pkgs.coreutils
    # curl does http requests. Comes with MacOS but no reason to use a dated
    # version.
    pkgs.curl
    # DNS troubleshooting / lookup.
    pkgs.dig
    # Show us the current system as a double.
    flake-inputs.current-system.packages.${system}.default
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
    # Lots of configurations or API calls use JSON payloads without whitespace.
    # While jq can do a lot more, in the admin context this can help us print
    # these things.
    pkgs.jq
    # I just thought this was built-in, but I guess not.  We need to
    # kill all processes by name sometimes!
    pkgs.killall
    # Allow creation, expansion, and inspection of ISO-9660 images.
    pkgs.libisoburn
    # Give us uuid and uuid-config.  This can be used to making deterministic
    # (typically v3) UUIDs.
    pkgs.libossp_uuid
    # Let us see ports and file handles held by processes.
    pkgs.lsof
    # A union of ping and traceroute - ping all hosts along a route.
    pkgs.mtr
    # Provides a series of networking tools, including arp.  On macOS this can
    # be essential because we want tools like ssh (from Nix) to use the same ARP
    # table.
    pkgs.nettools
    # Show greater detail from `nix build`, which is necessary for debugging
    # sometimes.  Use "nom build" in place of "nix build".
    pkgs.nix-output-monitor
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
    pkgs.speedtest-rs
    # Disk health monitoring and reporting tools.  Provides smartctl and smartd.
    pkgs.smartmontools
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
    # We need an editor.  Even if we don't want to edit configuration files
    # because we already have Nix, it's still helpful for things that use
    # EDTIOR, such as C-x C-e in the shell.  Can we just uninstall nano?
    pkgs.vim
    # A handy alternative to curl, best suited for downloading content.
    pkgs.wget
  ] ++ lib.optionals pkgs.stdenv.isLinux [
    # Allow normal, mortal user sessions to look at disks - it's crazy how hard
    # this is to do.
    pkgs.bindfs
    # Handy DNS lookup tool.
    pkgs.dig
    # Use for debugging network issues such as checking full or half duplex.
    pkgs.ethtool
    # Use for querying available hardware.  See also pciutils which provides
    # lspci.
    pkgs.lshw
    # Show network traffic and the associated processes.
    pkgs.nethogs
    # Partition editing.
    pkgs.parted
    # Use for querying available hardware.  See also lshw.
    pkgs.pciutils
    # Allow saved sessions and fancy terminal prestidigitation.  Even without
    # magic tricks, very useful for unstable transitions.
    pkgs.screen
    # Gives us iostat, pidstat, and sar.  Show us IO usage (such as disk
    # reads/writes).  Useful for identifying performance bottlenecks relating to
    # disk.  Use `sar -A` to show CPU, memory, and IO.  The hunt continues for
    # something also showing network traffic.
    pkgs.sysstat
    # Show the path that packets take.
    pkgs.traceroute
    # Adds a whole lot of tools, but I mostly just need lsblk.  This seems to be
    # implicitly installed on Linux hosts, but this will give it to us on macOS
    # too.  But it doesn't seem to install that for macOS.  Good to have it
    # explicitly defined anyways.
    pkgs.util-linux
  ];
  # Make suggestions for a failed command.  Similar to "thefuck", but in Rust
  # instead of Python, so it's really zippy.  It defaults to being aliased to
  # "f".
  programs.pay-respects.enable = true;
  imports = [
    # cyme isn't available on all versions of nixpkgs I use.
    (lib.mkIf (builtins.hasAttr "cyme" pkgs) {
      environment.systemPackages =
        if (builtins.hasAttr "cyme" pkgs)
        then [
          # Allows us to query the status of USB devices.  This uses lsusb or
          # systemprofile -json under the hood in a cross-platform manner.
          # Unfortunately it does not work on non-USB devices (like SD cards)
          # like one might think.  This is _not_ for storage devices (many
          # things imply it will work, but it won't).
          pkgs.cyme
        ]
        else []
      ;
    })
  ];
}
