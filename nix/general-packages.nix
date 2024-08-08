{ pkgs, ... }:
[
  # Write certificates out - these aren't present on macOS in a freely
  # available way.
  pkgs.cacert
  # A command line based music player.
  pkgs.cmus
  # (import (builtins.fetchGit {
  #   # Descriptive name to make the store path easier to identify
  #   name = "curl-8-1-0-fix-ssl";
  #   url = ~/dev/nixpkgs;
  #   ref = "master";
  #   rev = "8fcd9a317391dffeb4475f2f8810fd2b7c217ba4";
  # }) {}).curl
  # (import (builtins.fetchGit {
  #   # Descriptive name to make the store path easier to identify
  #   name = "crystal-1-0-fetch-git";
  #   url = https://github.com/nixos/nixpkgs/;
  #   rev = "3b6c3bee9174dfe56fd0e586449457467abe7116";
  # }) {}).crystal
  # A specialized charting tool using a declarative language. Supports a
  # specific set of charts but I don't remember which. Used by plantuml.
  pkgs.ditaa
  # Allow split DNS lookups while on the VPN, so I can still reach the
  # intranet.
  pkgs.dnsmasq
  # Detect character encoding of files.
  pkgs.enca
  # This is a binary apparently. Universal editor settings. Kind of like a
  # pre-prettier. I don't know the nix pacakge though, if it exists.
  #pkgs.editorconfig
  # Read/write speed tests using f3read and f3write.
  pkgs.f3
  # Convert media (sound, video). The "-full" suffix brings in all of the
  # codecs one could desire.
  # See https://github.com/NixOS/nixpkgs/issues/271313 for workaround to fix
  # ffmpeg segfaulting (signal 11) until this can make it to nixpkgs-unstable
  # (currently in staging as of 2024-01-08).
  # pkgs.ffmpeg-full
  (pkgs.ffmpeg-full.override {
    x264 = pkgs.x264.overrideAttrs (old: {
      postPatch = old.postPatch
                  + pkgs.lib.optionalString (pkgs.stdenv.isDarwin) ''
        substituteInPlace Makefile --replace '$(if $(STRIP), $(STRIP) -x $@)' '$(if $(STRIP), $(STRIP) -S $@)'
      '';
    });
  })
  pkgs.fuse-ext2
  # (import (builtins.fetchGit {
  #   # Descriptive name to make the store path easier to identify
  #   name = "nixpkgs-early-fuse-ext2";
  #   url = https://github.com/nixos/nixpkgs/;
  #   rev = "277ea6cac2fca11859ca8a29fafda7af681b4943";
  # }) {
  #   inherit system;
  # }).fuse-ext2
  pkgs.e2fsprogs
  # macOS doesn't ship with free.
  # pkgs.unixtools.free
  pkgs.utillinux
  # Read EXIF metadata from images.
  pkgs.exiftool
  pkgs.procps
  # The BSD awk I have found lacking. Not really a surprise. Replace it with
  # GNU.
  pkgs.gawk
  # Consume Github from the command line. Can make testing pull requests very
  # easy without needing to do a bunch of git remote management.
  pkgs.gh
  # Make pretty graphs from a declarative language.
  pkgs.gnuplot
  # A password storage system.
  pkgs.gnupg
  # A specialized charting tool using a declarative language. Supports a
  # large set of charts. Used by plantuml.
  pkgs.graphviz
  # Test unfree functionality in nix.
  pkgs.hello-unfree
  # An HTML validation tool.
  pkgs.html-tidy
  # For previewing LaTeX in Emacs.
  pkgs.imagemagick
  # Manage Jira from the command line, like a scholar.
  pkgs.jira-cli-go
  # JSON parsing, querying, and updating.
  pkgs.jq
  # Manage pesky ssh-agent and gpg-agent sessions universally.
  pkgs.keychain
  # "Office" tools.
  # Doesn't build on macOS yet.
  # pkgs.libreoffice
  # Give us man pages for GNU stuff.
  pkgs.man-pages
  # Email indexing, viewing, etc. Needed in general because it is required by
  # the Emacs config.  This does not install the mu4e package any longer due to
  # this: https://github.com/NixOS/nixpkgs/pull/253438
  # To work around this, make sure the package `emacs.pkgs.mu4e` is included.
  # See home.nix for how emacs is configured to include mu4e, plus additional
  # documentation on the configuration.
  pkgs.mu
  # This lets me search for parts of nix to help debug collisions.
  pkgs.nix-index
  # Provides `nixos-generate` which can be used to build ready-to-go images of
  # NixOS.  Useful for writing state to an SD card and putting into an Rasberry
  # Pi, for example.  The nixpkgs version lags behind a lot, so see overlays for
  # managing the specific version/rev.
  # pkgs.nixos-generators
  # Kept as reference until I can leverage a project templating tool that would
  # lay out a flake.nix for Node.js/TypeScript projects.
  # Language Server (Protocol) - a generic means of consuming languages in an
  # editor agnostic manner.
  # Lifted from https://code-notes.jhuizy.com/add-custom-npm-to-home-manager/
  # pkgs.nodePackages.typescript-language-server
  # Opens source VPN solution that can work in place for Cisco's AnyConnect and
  # possibly others.
  pkgs.openconnect
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
  (pkgs.pass.withExtensions (px: [
    # Manage TOTP passwords via pass.
    # Use with zbar to decode QR codes from images or webcams.
    # Example of using zbar to tie in auth with pass-otp:
    # zbarimg -q --raw qrcode.png | pass otp insert totp-secret
    # If the registration UI asks for a "verification code", just generate a
    # totp and use that.
    # See readme for other cool uses: https://github.com/tadfisher/pass-otp
    px.pass-otp
  ]))
  # A charting tool with a declarative language. Uses graphviz dot, ditaa,
  # and others.
  pkgs.plantuml
  # A containerd wrapper like Docker, but without the Docker branding stuff.
  # Note that on macOS this will be a wrapper for podman-machine, and a VM must
  # be installed on the host.  I don't really know the idiomatic Nix way of
  # doing this just yet.  See https://github.com/NixOS/nixpkgs/issues/169118 for
  # when it was introduced/fixed for macOS.  Note that per
  # https://github.com/skogsbrus/os/commit/2b10be2d1a48727470a72f9e7b1f307d327adee3
  # it seems to need qemu installed too, though how these packages know about
  # each other is not something I understand at present.
  pkgs.podman
  # Query an HTML DOM from the command line.
  pkgs.pup
  # Like Ruby, but not.
  (pkgs.python3.withPackages (ps: [
    ps.lxml
    #ps.pyqt5
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
  # qemu is a is an open source virtualizer. This contributes to a successful
  # podman installation on macOS.  How one detects the other and does the
  # needful is not clear, but perhaps it is not necessary at install/compile
  # time and thus my quizzical thoughts on it are unwarranted.  See the podman
  # package declaration for more information.
  pkgs.qemu
  # Change encoding. Can convert HTML entities.
  pkgs.recode

  # Screenkey displays what's being typed on the screen. Too bad it doesn't
  # work on aarch64. For macOS, keycastr https://github.com/keycastr/keycastr
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
  (pkgs.openssh.override { withKerberos = true; })
  # I found this in the openssh/default.nix file.  Doesn't build on macOS yet
  # due to a missing "security framework":
  # > configure: error: *** Need a security framework to use the credentials cache API ***
  # This needs more investigation.  No ticket searching has been done yet.
  # pkgs.openssh_gssapi
  # A lightweight SQL database which requires no server. This also installs
  # CLI tools in which to access SQLite databases.
  pkgs.sqlite
  # Gives us GNU's makeinfo, which I use to build org-mode successfully. This
  # is because the make target eventually invokes `info', which expects a gnu
  # makeinfo. The BSD makeinfo has "mismatch" errors.
  # Disabled to work out a collision:
  # error: collision between `/nix/store/dpjw0569z88lldqfn2sq4hckgwvrm1i1-texinfo-7.0.3/bin/pod2texi' and `/nix/store/h3iw2viz7g98xy7qj9yfvm688fvwlla7-texinfo-interactive-7.0.3/bin/pod2texi'
  # pkgs.texinfo
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
      # Business cards.
      bizcard
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
  # Some folks still use rar for an archive format. This lets us decompress
  # those archives.
  pkgs.unrar
  # Openconnect pulls this in, but declaring it here makes it easy for us
  # to use the vpnc-script that comes with it.
  #
  # Oh, but we can't build it directly on macOS. wtf :(
  # And its binary isn't available from the help I've discovered.
  #
  #
  # Disabled until this is supported on Darwin or I figure out how to debug the
  # clang issues (see overlays/vpnc.nix for more info).
  # pkgs.vpnc
  # The open source version of VSCode is VSCodium (a play on Chromium, I
  # suppose). Doesn't install on aarch64 yet. I want this package so I can
  # work with those who have not come to Holy Emacs yet.
  # pkgs.vscodium
  # Renders HTML formatted emails.
  pkgs.w3m
  # Sign Firefox extensions authored by me.  This was hard to find so I want to
  # keep a reference of it, but it should be moved to a project's flake.nix
  # which needs extensions to be signed.  I don't have such a project currently.
  # pkgs.nodePackages.web-ext
  # Run Windows programs (sometimes even I need this).
  # Doesn't work on Darwin...? But it worked on my other machine. I need to sync
  # versions.
  # pkgs.wine
  # The wireguard-go package must be installed separatedly, even tough
  # wg-quick (from wireguard-tools) depends upon wireguard-go.
  pkgs.wireguard-go
  # Folks suggest installing an application from the AppStore. I'd prefer
  # to have an unattended install of the Wireguard client.
  pkgs.wireguard-tools
  # Allow extracting and creating .xz archives.
  pkgs.xz
  # Decode QR codes from images or an onboard camera (such as a webcam).
  # Example of using zbar to tie in auth with pass-otp:
  # zbarimg -q --raw qrcode.png | pass otp insert totp-secret
  # Potentially more docs in the pass + otp section of this file.
  # If the registration UI asks for a "verification code", just generate a
  # totp and use that.
  pkgs.zbar
  # A better interactive shell than bash, I think.
  pkgs.zsh
  # Highlight zsh syntax.
  pkgs.zsh-syntax-highlighting
  # Used by Emacs' Tramp to handle compression over SSH.
  pkgs.zstd
]
