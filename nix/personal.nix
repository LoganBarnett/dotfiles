{ pkgs, ... }:
[
  # These are a suggestion from https://stackoverflow.com/a/51161923 to get
  # clang behaving properly for some trouble I was having with lxml (a Python
  # package).
  #
  # pkgs.darwin.apple_sdk.frameworks.Security
  # pkgs.darwin.apple_sdk.frameworks.CoreFoundation
  # pkgs.darwin.apple_sdk.frameworks.CoreServices
  #
  # 3D modeling, but without the indentured servitude.
  #pkgs.blender
  # gem-apps
  # Compile natively to tiny devices.
  # Actually I'm not sure what to use. Arduino has no Darwin/macOS version
  # and avr-gcc no longer exists. I can't even find a ticket on it.
  # Perhaps I should file one?
  # arduino
  # avr-gcc
  # Convert media (sound, video). The "-full" suffix brings in all of the
  # codecs one could desire.
  pkgs.ffmpeg-full
  # Rasterized image manipulation.
  #pkgs.gimp
  # Haskell docs say this is necessary for Haskell. See
  # https://docs.haskellstack.org/en/v0.1.10.1/nix_integration.html#using-a-custom-shell-nix-file
  # See Haskell entry for why I'm not bothering.
  # glpk

  # Haskell's install information is hard to get a hold of or seems to
  # assume an existing Stack installation. None of these seem appealing to
  # me nor in the spirit of Nix's declarative package management. For now
  # Haskell can take a back seat since I don't use it much anyways.
  # haskell.packages.lts-3_13.ghc
  # Brings in telnet. Similar to netcat - has its uses as a very bare-bones
  # network communication tool.
  pkgs.inetutils
  # Email gathering and sending. Works with mu.
  pkgs.isync
  # Online service that works as proxy for quick, local sockets.
  pkgs.ngrok
  # Make it easy to try out Nix packages under review.
  pkgs.nixpkgs-review
  # Can show media info for files using a codec. Similar to ffmpeg's ffprobe.
  pkgs.mediainfo
  # A program to allow communicating over serial. Perhaps I can script it?
  pkgs.minicom
  # Like nmap, a tool for testing network ports. The executable is "nc".
  # This installs the GNU version, although the OpenBSD version allows
  # connecting to Unix sockets. For Unix sockets just use socat.
  pkgs.netcat
  # Connect to MongoDB for poking around the document store.
  # Currently broken due to gperftools.
  # mongodb
  # Recursively walks up the file hiearchy to show permissions. Quite helpful!
  # Currently not available as a nix package. Research on this has led to
  # attempting unixtools and nettools. The unixtools package doesn't contain
  # namei, and I could not confirm nettools due to an issue with openssl being
  # out of date in that package.
  #
  # Currently broken on aarch64. No tickets found on it. Looks like xnu is
  # trying to use x86_64 and breaking.
  #pkgs.nettools
  # A tool for mapping network ports.
  pkgs.nmap
  # OBS does sweet screen recording and video composition.
  # Sadly, this does not work on Darwin, yet.
  # pkgs.obs-studio
  # Used to do split tunneled VPN connections from the command line. You can
  # also, you know, download it. Looking at you, AnyConnect.
  #pkgs.openconnect
  # openconnect-sso wraps openconnect to provide SSO functionality.
  #
  # (pkgs.callPackage
  #   "${builtins.fetchTarball https://github.com/vlaci/openconnect-sso/archive/master.tar.gz}/nix/openconnect-sso.nix"
  #   {
  #     lib = pkgs.lib;
  #     openconnect = pkgs.openconnect;
  #     python3 = pkgs.python39;
  #     python3Packages = pkgs.python39Packages;
  #     poetry2nix = pkgs.poetry2nix;
  #     substituteAll = pkgs.substituteAll;
  #     wrapQtAppsHook = pkgs.libsForQt5.wrapQtAppsHook;
  #   }
  # )
  #
  # (((pkgs.callPackage ./openconnect-sso.nix { pkgs = pkgs; }) {}) {
  #                                               lib = pkgs.lib;
  #                                               openconnect = pkgs.openconnect;
  #                                               python3 = pkgs.python39;
  #                                               python3Packages = pkgs.python39Packages;
  #                                               poetry2nix = pkgs.poetry2nix;
  #                                               substituteAll = pkgs.substituteAll;
  #                                               wrapQtAppsHook = pkgs.wrapQtAppsHook;
  #                                             })
  #
  # CAD software with a programming language behind it. Declarative models!
  # pkgs.openscad
  # I get build errors with 2021-01.
  # TODO: Report the build errors.
  #(import (builtins.fetchGit {
  #  # Descriptive name to make the store path easier to identify
  #  name = "nixpkgs-pre-pr-111997";
  #  url = https://github.com/nixos/nixpkgs/;
  #  rev = "385fc8362b8abe5aa03f50c60b9a779ce721db19";
  #}) {lib = lib;}).openscad
  #pkgs.openscad

  # See also oh-my-zsh.
  # Experimental attempt at getting sshfs working. This is not adequate to get
  # a functional sshfs. But this successfully builds so I wouldn't be far away
  # from creating a darwin build for sshfs on nixpkg.  Can be manually
  # installed from https://osxfuse.github.io/
  pkgs.osxfuse
  # For pairing. But probably not ready.
  (pkgs.callPackage ./pair-ls.nix { } )
  # Because sometimes you need something better than grep.
  pkgs.perl
  # Haskell docs say this is necessary for Haskell. See
  # https://docs.haskellstack.org/en/v0.1.10.1/nix_integration.html#using-a-custom-shell-nix-file
  # See Haskell entry for why I'm not bothering.
  # pcre
  # A build tool chain for microcontrollers and other embedded platforms.
  # Works for Ardunio.
  # pkgs.platformio
  # podman is an alternative to docker - supposed to be more or less fully
  # compatible.
  # I could install this, but on macOS it's actually podman-remote. This is
  # not useful to me.
  # pkgs.podman
  # (pkgs.python2.withPackages (ps: [
]
