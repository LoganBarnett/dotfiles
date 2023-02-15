{ pkgs, ... }:
[
  # A grep-sed like alternative. Offers a scripting language for
  # transformations.
  pkgs.ack
  # Convert Ruby gems to valid nix derivations or Ruby dependencies within
  # Nix. Or that's the idea. While this builds, I haven't accomplished what it
  # says on the tin just yet.
  pkgs.bundix
  # Write certificates out - these aren't present on macOS in a freely
  # available way.
  pkgs.cacert
  # This is a suite of GNU utilities.
  pkgs.coreutils
  # curl does http requests. Comes with MacOS but no reason to use a dated
  # version.
  pkgs.curl
  # (import (builtins.fetchGit {
  #   # Descriptive name to make the store path easier to identify
  #   name = "crystal-1-0-fetch-git";
  #   url = https://github.com/nixos/nixpkgs/;
  #   rev = "3b6c3bee9174dfe56fd0e586449457467abe7116";
  # }) {}).crystal
  # Gives us diff --color support via GNU diff.
  pkgs.diffutils
  # Just loads and unloads environment variables based on directory. Really
  # useful with nix to declare local dependencies for a project, and see them
  # auto loaded when entering the directory. See the local shell configuration
  # for how direnv is hooked up.
  #
  # See more at https://nixos.wiki/wiki/Development_environment_with_nix-shell
  pkgs.direnv
  # A specialized charting tool using a declarative language. Supports a
  # specific set of charts but I don't remember which. Used by plantuml.
  pkgs.ditaa
  # Allow split DNS lookups while on the VPN, so I can still reach the
  # intranet.
  pkgs.dnsmasq
  # The ultimate editor. But how to make it conditional on OS?
  # pkgs.emacsMacport
  # aarch64 (arm) is lacking on the mainline build of emacs/emacsMacPort. This
  # branch builds. See https://github.com/NixOS/nixpkgs/pull/138424 for
  # progress on it getting merged.
  #(import (pkgs.fetchFromGitHub {
  #  # Descriptive name to make the store path easier to identify
  #  name = "emacs-macport-m1";
  #  owner = "mikroskeem";
  #  repo = "nixpkgs";
  #  rev = "84e8bdba798312ace0e854f885cb76a5ddad1101";
  #  sha256 = "iij2TsujINT/hgIHn3epchE09hWGkPk8UdtwFBjdAsU=";
  #}) {}).emacsMacport
  pkgs.emacs
  # This is a binary apparently. Universal editor settings. Kind of like a
  # pre-prettier. I don't know the nix pacakge though, if it exists.
  #pkgs.editorconfig
  # Searches for files. Used by projectile in Emacs.
  pkgs.fd
  # The BSD awk I have found lacking. Not really a surprise. Replace it with
  # GNU.
  pkgs.gawk
  # Consume Github from the command line. Can make testing pull requests very
  # easy without needing to do a bunch of git remote management.
  pkgs.gh
  # git manages my code. Comes with MacOS but no reason to use a dated
  # version.
  pkgs.git
  # Who wants BSD grep when you could have GNU grep?
  pkgs.gnugrep
  # Make pretty graphs from a declarative language.
  pkgs.gnuplot
  # A password storage system.
  pkgs.gnupg
  # Who wants BSD sed when you could have GNU sed?
  pkgs.gnused
  # Who wants BSD ls when you could have GNU ls? And other GNU things.
  pkgs.gnutls
  # A specialized charting tool using a declarative language. Supports a
  # large set of charts. Used by plantuml.
  pkgs.graphviz
  # An HTML validation tool.
  pkgs.html-tidy
  # A more visual version of top.
  pkgs.htop
  # IBM font that looks good outside of source code contexts.
  pkgs.ibm-plex
  # For previewing LaTeX in Emacs.
  pkgs.imagemagick
  # A spell checker.
  pkgs.ispell
  # I think I have some extra memory. Java should take care of that.  For now
  # this seems broken - I suspect it is a collision with maven.  See
  # https://github.com/NixOS/nixpkgs/issues/56549 - my local version also
  # seems to be Zulu, for what it's worth.
  # jdk
  # Managed Java on a per-project basis. Doesn't exist in nix?
  # jenv
  # JSON parsing, querying, and updating.
  pkgs.jq
  # Manage pesky ssh-agent and gpg-agent sessions universally.
  pkgs.keychain
  # "Office" tools.
  # Doesn't build on macOS yet.
  # pkgs.libreoffice
  # Give us man pages for GNU stuff.
  pkgs.man-pages
  # Java builds. Pom.xml files as far as the eyes can see.
  pkgs.maven
  # A union of ping and traceroute - ping all hosts along a route.
  pkgs.mtr
  # This lets me search for parts of nix to help debug collisions.
  pkgs.nix-index
  # The code injector bootstrap script is written in Node.js, so we need to
  # run it from the command line.
  pkgs.nodejs
  # Language Server (Protocol) - a generic means of consuming languages in an
  # editor agnostic manner.
  # Lifted from https://code-notes.jhuizy.com/add-custom-npm-to-home-manager/
  pkgs.nodePackages.typescript-language-server
  # Elastic, load balanced, self hosted, containerized server pools. This is
  # the command line interface.
  pkgs.nomad
  # oq is like jq but for xml. It parses xml and yaml files and can convert
  # them to xml, yaml, or json (with jq as a backend). It uses the same snytax
  # as jq with the exception of the -i and -o arguments to indicate which
  # format is to be used. It just uses jq internally for all translations.
  #
  # Broken at the moment. Need to file bug.
  # pkgs.oq
  # Translate human documents from one format to another.
  pkgs.pandoc
  # Manage passwords using gpg.
  pkgs.pass
  # Use Unix pipes and direct them to a human program.
  #pkgs.percol
  # An interface for reading passwords.
  pkgs.pinentry
  # A charting tool with a declarative language. Uses graphviz dot, ditaa,
  # and others.
  pkgs.plantuml
  # Query an HTML DOM from the command line.
  pkgs.pup
  # Disabled until allowBroken is addressed.
  # See https://github.com/mooz/percol/issues/110
  #
  # ps.percol
  # ]))
  # Like Ruby, but not.
  (pkgs.python39.withPackages (ps: [
    ps.pip
    ps.lxml
    #ps.pyqt5
    # (ps.buildPythonPackage rec {
    #   pname = "openconnect-sso";
    #   version = "0.7.3";
    #   # disabled = isPy27;

    #   src = pkgs.python39.pkgs.fetchPypi {
    #     inherit pname version;
    #     sha256 = "065s5c8q80jh0psdw7694nlabwpra7aw6yc4jlgsc9vxx8rx2na1";
    #   };

    #   propagatedBuildInputs = [ ps.attrs ];
    #   doCheck = false;
    #   # checkInputs = [ pytest ];
    #   # checkPhase = ''
    #   #   py.test -m "not webtest"
    #   # '';

    #   meta = with pkgs.python39.lib; {
    #     description = "Wrapper script for OpenConnect supporting Azure AD (SAMLv2) authentication to Cisco SSL-VPNs.";
    #     homepage = "https://github.com/vlaci/openconnect-sso";
    #     # license = licenses.asl20;
    #     # maintainers = with maintainers; [ flokli ];
    #   };
    # })
    # This does nothing.
    # PyQt5
    # This does something, but breaks.
    # (ps.buildPythonPackage rec {
    #   pname = "PyQt5";
    #   version = "5.15.4";
    #   # disabled = isPy27;

    #   src = pkgs.python39.pkgs.fetchPypi {
    #     inherit pname version;
    #     sha256 = "1gp5jz71nmg58zsm1h4vzhcphf36rbz37qgsfnzal76i1mz5js9a";
    #   };

    #   buildPhase = "${pkgs.python39.interpreter} project.py build";

    #   propagatedBuildInputs = [];
    #   doCheck = false;
    #   # checkInputs = [ pytest ];
    #   # checkPhase = ''
    #   #   py.test -m "not webtest"
    #   # '';

    #   meta = with pkgs.python39.lib; {
    #     description = "PyQt is a set of Python bindings for The Qt Company's Qt application framework.";
    #     homepage = "https://www.riverbankcomputing.com/software/pyqt/";
    #     # license = licenses.asl20;
    #     # maintainers = with maintainers; [ flokli ];
    #   };
    # })
  ]))
  # pkgs.pyqtbuild
  # pkgs.qt5Full
  # PyQt5
  # Doesn't exist?
  # rbenv
  # Really fast grep alternative.
  pkgs.ripgrep
  # Copy files recursively. Replaces BSD version on macOS.
  pkgs.rsync
  # Like Python, but not.
  # DO NOT RUN RUBY IN NIX. NIX IS NOT READY FOR RUBY.
  # ruby

  # Rust is a memory safe, type safe, performant language.
  # rust-src is needed for rustracer to be fully functional.
  # (pkgs.rustChannels.stable.rust.override { extensions = ["rust-src"]; })
  # Racer is an auto-complete tool for Rust.
  #pkgs.rustracer

  # A Language Server (LSP) implementation for Rust
  pkgs.rust-analyzer
  # A version management tool for Rust.
  pkgs.rustup
  # Assume an account on AWS via SAML.
  pkgs.saml2aws
  # Screenkey displays what's being typed on the screen. Too bad it doesn't
  # work on aarch64. For macOs, keycastr https://github.com/keycastr/keycastr
  # works great, but is installed via homebrew's cask.
  # pkgs.screenkey
  # Needed to do Crystal development, or really when I want to contribute to
  # oq.
  #
  # Broken for the moment, just like oq on nix.
  #
  # TODO: Report the build errors.
  # (import (builtins.fetchGit {
  #   # Descriptive name to make the store path easier to identify
  #   name = "nixpkgs-pre-pr-115471";
  #   url = https://github.com/nixos/nixpkgs/;
  #   rev = "29b0d4d0b600f8f5dd0b86e3362a33d4181938f9";
  # }) {}).shards
  # pkgs.shards
  # Run speed tests from the command line.
  pkgs.speedtest-cli
  # A self-proclaimed better netcat.
  pkgs.socat
  # A good mono-spaced font that is largely sans-serif, but uses serifs to
  # disambuguate.
  pkgs.source-code-pro
  # A lightweight SQL database which requires no server. This also installs
  # CLI tools in which to access SQLite databases.
  pkgs.sqlite
  # Declarative server orchestration.
  pkgs.terraform
  # Gives us GNU's makeinfo, which I use to build org-mode successfully. This
  # is because the make target eventually invokes `info', which expects a gnu
  # makeinfo. The BSD makeinfo has "mismatch" errors.
  pkgs.texinfo
  # Binary for incremental parsing of various languages for Emacs.
  pkgs.tree-sitter
  # LaTeX is for rendering a number of diagrams and documents.
  # scheme-full is way too big. Use scheme-basic and pull in other modules
  # selectively. See
  # https://nixos.org/manual/nixpkgs/stable/#sec-language-texlive for
  # additional information on LaTeX installation.
  # pkgs.texlive.combined.scheme-basic
  (pkgs.texlive.combine {
    inherit (pkgs.texlive)
      algorithms
      capt-of
      # Circuit diagram support.
      circuitikz
      scheme-small
      # SI units (metric?).
      siunitx
      wrapfig
    ;
  })
  # Swear at the command line to make it guess what you really meant to type,
  # or just do the needful after a silly command that tells you what you
  # _should_ have done.
  pkgs.thefuck
  # Highly controllable terminal emulation and session management.
  pkgs.tmux
  # Some folks still use rar for an archive format. This lets us decompress
  # those archives.
  pkgs.unrar
  # Openconnect pulls this in, but declaring it here makes it easy for us
  # to use the vpnc-script that comes with it.
  #
  # Oh, but we can't build it directly on macOS. wtf :(
  # And its binary isn't available from the help I've discovered.
  #
  # vpnc
  # The penultimate editor.
  pkgs.vim
  # The open source version of VSCode is VSCodium (a play on Chromium, I
  # suppose). Doesn't install on aarch64 yet. I want this package so I can
  # work with those who have not come to Holy Emacs yet.
  # pkgs.vscodium
  # Renders HTML formatted emails.
  pkgs.w3m
  # Sign Firefox extensions authored by me.
  pkgs.nodePackages.web-ext
  # A handy alternative to curl, best suited for downloading content.
  pkgs.wget
  # The wireguard-go package must be installed separatedly, even tough
  # wg-quick (from wireguard-tools) depends upon wireguard-go.
  pkgs.wireguard-go
  # Folks suggest installing an application from the AppStore. I'd prefer
  # to have an unattended install of the Wireguard client.
  pkgs.wireguard-tools
  # A tool for installing node packages. Better than npm in most ways.
  pkgs.yarn
  # A better interactive shell than bash, I think.
  pkgs.zsh
  # Highlight zsh syntax.
  pkgs.zsh-syntax-highlighting
  # Used by Emacs' Tramp to handle compression over SSH.
  pkgs.zstd
]
