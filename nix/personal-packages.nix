{ pkgs, ... }:
[
  # My tool for directly updating BattleScribe's data from the source.
  pkgs.battlescribe-update-data
  # Manage bluetooth settings easily from the command line.
  pkgs.blueutil
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
  # A command line based music player.
  pkgs.cmus
  # gem-apps
  # Compile natively to tiny devices.
  # Actually I'm not sure what to use. Arduino has no Darwin/macOS version
  # and avr-gcc no longer exists. I can't even find a ticket on it.
  # Perhaps I should file one?
  # arduino
  # avr-gcc
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
  # WYSIWYG SVG editing, but also can render SVGs to PNGs from the command line.
  pkgs.inkscape
  # Email gathering and sending. Works with mu.
  pkgs.isync
  # Online service that works as proxy for quick, local sockets.
  pkgs.ngrok
  # Make it easy to try out Nix packages under review.
  pkgs.nixpkgs-review
  # Experimental attempt at getting sshfs working. This is not adequate to get
  # a functional sshfs. But this successfully builds so I wouldn't be far away
  # from creating a darwin build for sshfs on nixpkg.  Can be manually
  # installed from https://osxfuse.github.io/
  # This was historically called osxfuse.
  pkgs.macfuse-stubs
  # Can show media info for files using a codec. Similar to ffmpeg's ffprobe.
  pkgs.mediainfo
  # Email indexing, viewing, etc. Needed in general because it is required by
  # the Emacs config.  This does not install the mu4e package any longer due to
  # this: https://github.com/NixOS/nixpkgs/pull/253438
  # To work around this, make sure the package `emacs.pkgs.mu4e` is included.
  # See home.nix for how emacs is configured to include mu4e, plus additional
  # documentation on the configuration.
  pkgs.mu
  # OBS does sweet screen recording and video composition.
  # Sadly, this does not work on Darwin, yet.
  # pkgs.obs-studio
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
  # I get build errors with 2021-01.
  # TODO: Report the build errors.
  #(import (builtins.fetchGit {
  #  # Descriptive name to make the store path easier to identify
  #  name = "nixpkgs-pre-pr-111997";
  #  url = https://github.com/nixos/nixpkgs/;
  #  rev = "385fc8362b8abe5aa03f50c60b9a779ce721db19";
  #}) {lib = lib;}).openscad
  # CAD software with a programming language behind it. Declarative models!
  # pkgs.openscad
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
  # Deploy things in my network.
  pkgs.proton-deploy
  # (pkgs.python2.withPackages (ps: [
  # Run Windows programs (sometimes even I need this).

  # (pkgs.callPackage
  #   "${builtins.fetchTarball https://github.com/nixos/nixpkgs/archive/master.tar.gz}/nix/openconnect-sso.nix"
  #   {
  #     lib = pkgs.lib;
  #     # openconnect = pkgs.openconnect;
  #     # python3 = pkgs.python39;
  #     # python3Packages = pkgs.python39Packages;
  #     # poetry2nix = pkgs.poetry2nix;
  #     # substituteAll = pkgs.substituteAll;
  #     # wrapQtAppsHook = pkgs.libsForQt5.wrapQtAppsHook;
  #   }
  # )
  # pkgs.pkgsx86_64Darwin.wine-wow64
  # pkgs.wine-wow64
  # pkgs.wow64Experimental
  # This was my last attempt, but I don't think it entirely worked.
  # pkgs.pkgsx86_64Darwin.wine64
  # For working with zst archives.
  pkgs.zstd
]
