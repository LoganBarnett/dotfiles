{ pkgs, lib, ... }:
let
  # PyQt5 = pkgs.callPackage ./PyQt5.nix;
  # PyQt5 = (import ./PyQt5.nix);

  # python39 = pkgs.callPackage ./PyQt5.nix;
  # openconnect-sso-src = builtins.fetchTarball "https://github.com/vlaci/openconnect-sso/archive/master.tar.gz";
  nixpkgs.overlays = [
    # (import ./overlays/crystal.nix)
    (import ./overlays/gnupg.nix)
    (import ./overlays/maven.nix)
    (import ./overlays/percol.nix)
    (import ./overlays/speedtest-cli.nix)
    (import ./overlays/zsh.nix)
    # (import (builtins.fetchTarball
    #   "https://github.com/oxalica/rust-overlay/archive/master.tar.gz"))
    # (import "${openconnect-sso-src}/overlay.nix")
    # (import ./overlays/openconnect-sso.nix)
    # (import ./openconnect-sso.nix)
    # (import "${builtins.fetchTarball https://github.com/vlaci/openconnect-sso/archive/master.tar.gz}/overlay.nix")
  ];

in
{

  # TODO: Disable this... some time? Would be nice to know exactly what is
  # depending on it. Reading here
  # https://discourse.nixos.org/t/how-to-override-openssl-q-a/9964/3 indicates
  # that a simple upgrade is not possible, and that the dependers must change
  # how they consume OpenSSL.
  #
  # This must not be placed within the let block or it just doesn't work. Why
  # this behavior exists is unknown to me.
  nixpkgs.config = {
    # Some packages are not "free". We need to specifically bless those.
    allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
      "unrar"
    ];
    # Somehow this can get lost, and I'm not convinced this is home-managers'
    # nor nix's doing. That said, this setting seems to have no effect.
    networking.hostname = "neon.proton";
    permittedInsecurePackages = [
      "openssl-1.0.2u"
    ];
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "logan";
  home.homeDirectory = "/Users/logan";

  home.packages = [
    # These are a suggestion from https://stackoverflow.com/a/51161923 to get
    # clang behaving properly for some trouble I was having with lxml (a Python
    # package).
    #
    # pkgs.darwin.apple_sdk.frameworks.Security
    # pkgs.darwin.apple_sdk.frameworks.CoreFoundation
    # pkgs.darwin.apple_sdk.frameworks.CoreServices
    #
    # A grep-sed like alternative. Offers a scripting language for
    # transformations.
    pkgs.ack
    # For running AWS commands. Generally this is just a backend for saml2aws
    # under my usage.
    pkgs.awscli
    # 3D modeling, but without the indentured servitude.
    pkgs.blender
    # Convert Ruby gems to valid nix derivations or Ruby dependencies within
    # Nix. Or that's the idea. While this builds, I haven't accomplished what it
    # says on the tin just yet.
    pkgs.bundix
    # gem-apps
    # Compile natively to tiny devices.
    # Actually I'm not sure what to use. Arduino has no Darwin/macOS version
    # and avr-gcc no longer exists. I can't even find a ticket on it.
    # Perhaps I should file one?
    # arduino
    # avr-gcc
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
    # A specialized charting tool using a declarative language. Supports a
    # specific set of charts but I don't remember which. Used by plantuml.
    pkgs.ditaa
    # Allow split DNS lookups while on the VPN, so I can still reach the
    # intranet.
    pkgs.dnsmasq
    # The ultimate editor. But how to make it conditional on OS?
    # emacs
    pkgs.emacsMacport
    # This is a binary apparently. Universal editor settings. Kind of like a
    # pre-prettier. I don't know the nix pacakge though, if it exists.
    # editorconfig
    # Searches for files. Used by projectile in Emacs.
    pkgs.fd
    # Convert media (sound, video). The "-full" suffix brings in all of the
    # codecs one could desire.
    pkgs.ffmpeg-full
    # The BSD awk I have found lacking. Not really a surprise. Replace it with
    # GNU.
    pkgs.gawk
    # Consume Github from the command line. Can make testing pull requests very
    # easy without needing to do a bunch of git remote management.
    pkgs.gh
    # Rasterized image manipulation.
    pkgs.gimp
    # git manages my code. Comes with MacOS but no reason to use a dated
    # version.
    pkgs.git
    # Haskell docs say this is necessary for Haskell. See
    # https://docs.haskellstack.org/en/v0.1.10.1/nix_integration.html#using-a-custom-shell-nix-file
    # See Haskell entry for why I'm not bothering.
    # glpk
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

    # Haskell's install information is hard to get a hold of or seems to
    # assume an existing Stack installation. None of these seem appealing to
    # me nor in the spirit of Nix's declarative package management. For now
    # Haskell can take a back seat since I don't use it much anyways.
    # haskell.packages.lts-3_13.ghc
    # An HTML validation tool.
    pkgs.html-tidy
    # A more visual version of top.
    pkgs.htop
    # For previewing LaTeX in Emacs.
    pkgs.imagemagick
    # A spell checker.
    pkgs.ispell
    # Email gathering and sending. Works with mu.
    pkgs.isync
    # I think I have some extra memory. Java should take care of that.  For now
    # this seems broken - I suspect it is a collision with maven.  See
    # https://github.com/NixOS/nixpkgs/issues/56549 - my local version also
    # seems to be Zulu, for what it's worth.
    # jdk
    # Managed Java on a per-project basis. Doesn't exist in nix?
    # jenv
    # JSON parsing, querying, and updating.
    pkgs.jq
    # For rendering a number of diagrams and documents.
    #latex
    # "Office" tools.
    # Doesn't build on macOS yet.
    # pkgs.libreoffice
    # Make it easy to try out Nix packages under review.
    pkgs.nixpkgs-review
    # Give us man pages for GNU stuff.
    pkgs.manpages
    # Java builds. Pom.xml files as far as the eyes can see.
    pkgs.maven
    # Can show media info for files using a codec. Similar to ffmpeg's ffprobe.
    pkgs.mediainfo
    # A program to allow communicating over serial. Perhaps I can script it?
    pkgs.minicom
    # A union of ping and traceroute - ping all hosts along a route.
    pkgs.mtr
    # Email indexing, viewing, etc.
    pkgs.mu
    # Like nmap, a tool for testing network ports. The executable is "nc".
    # This installs the GNU version, although the OpenBSD version allows
    # connecting to Unix sockets. For Unix sockets just use socat.
    pkgs.netcat
    # Connect to MongoDB for poking around the document store.
    # Currently broken due to gperftools.
    # mongodb
    # This lets me search for parts of nix to help debug collisions.
    pkgs.nix-index
    # Recursively walks up the file hiearchy to show permissions. Quite helpful!
    # Currently not available as a nix package. Research on this has led to
    # attempting unixtools and nettools. The unixtools package doesn't contain
    # namei, and I could not confirm nettools due to an issue with openssl being
    # out of date in that package.
    # namei
    # A tool for mapping network ports.
    pkgs.nmap
    # Elastic, load balanced, self hosted, containerized server pools. This is
    # the command line interface.
    pkgs.nomad
    # The code injector bootstrap script is written in Node.js, so we need to
    # run it from the command line.
    pkgs.nodejs
    # OBS does sweet screen recording and video composition.
    # Sadly, this does not work on Darwin, yet.
    # pkgs.obs-studio
    # Used to do split tunneled VPN connections from the command line. You can
    # also, you know, download it. Looking at you, AnyConnect.
    pkgs.openconnect
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
    # openscad
    # I get build errors with 2021-01.
    # TODO: Report the build errors.
    (import (builtins.fetchGit {
      # Descriptive name to make the store path easier to identify
      name = "nixpkgs-pre-pr-111997";
      url = https://github.com/nixos/nixpkgs/;
      rev = "385fc8362b8abe5aa03f50c60b9a779ce721db19";
    }) {}).openscad
    # oq is like jq but for xml. It parses xml and yaml files and can convert
    # them to xml, yaml, or json (with jq as a backend). It uses the same snytax
    # as jq with the exception of the -i and -o arguments to indicate which
    # format is to be used. It just uses jq internally for all translations.
    #
    # Broken at the moment. Need to file bug.
    # pkgs.oq

    # See also oh-my-zsh.
    # Experimental attempt at getting sshfs working. This is not adequate to get
    # a functional sshfs. But this successfully builds so I wouldn't be far away
    # from creating a darwin build for sshfs on nixpkg.  Can be manually
    # installed from https://osxfuse.github.io/
    pkgs.osxfuse
    # Translate human documents from one format to another.
    pkgs.pandoc
    # Manage passwords using gpg.
    pkgs.pass
    # Use Unix pipes and direct them to a human program.
    pkgs.percol

    # Because sometimes you need something better than grep.
    pkgs.perl
    # Haskell docs say this is necessary for Haskell. See
    # https://docs.haskellstack.org/en/v0.1.10.1/nix_integration.html#using-a-custom-shell-nix-file
    # See Haskell entry for why I'm not bothering.
    # pcre
    # An interface for reading passwords.
    pkgs.pinentry
    # A charting tool with a declarative language. Uses graphviz dot, ditaa,
    # and others.
    pkgs.plantuml
    pkgs.podman
    # Query an HTML DOM from the command line.
    pkgs.pup
    # (python2.withPackages (ps: [
    # Disabled until allowBroken is addressed.
    # See https://github.com/mooz/percol/issues/110
    #
    # ps.percol
    # ]))
    # Like Ruby, but not.
    (pkgs.python39.withPackages (ps: [
      ps.pip
      ps.lxml
      ps.pyqt5
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
    # Like Python, but not.
    # DO NOT RUN RUBY IN NIX. NIX IS NOT READY FOR RUBY.
    # ruby

    # Rust is a memory safe, type safe, performant language.
    # rust-src is needed for rustracer to be fully functional.
    # (pkgs.rustChannels.stable.rust.override { extensions = ["rust-src"]; })
    # Racer is an auto-complete tool for Rust.
    pkgs.rustracer

    # A version management tool for Rust.
    pkgs.rustup
    # Assume an account on AWS via SAML.
    pkgs.saml2aws
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
    # A lightweight SQL database which requires no server. This also installs
    # CLI tools in which to access SQLite databases.
    pkgs.sqlite
    # Similar to netcat - has its uses as a very bare-bones network
    # communication tool.
    pkgs.telnet
    # Declarative server orchestration.
    pkgs.terraform
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
    # Renders HTML formatted emails.
    pkgs.w3m
    # Sign Firefox extensions authored by me.
    pkgs.nodePackages.web-ext
    # A handy alternative to curl, best suited for downloading content.
    pkgs.wget
    # Folks suggest installing an application from the AppStore. I'd prefer
    # to have an unattended install of the Wireguard client.
    pkgs.wireguard-tools
    # A tool for installing node packages. Better than npm in most ways.
    pkgs.yarn
    pkgs.zsh
    pkgs.zsh-syntax-highlighting
    # Used by Emacs' Tramp to handle compression over SSH.
    pkgs.zstd
  ];
  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";

}
