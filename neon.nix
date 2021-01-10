{ pkgs, ... }:
{
  nixpkgs.config.packageOverrides = pkgs: {
    gnupg = pkgs.gnupg.overrideAttrs {
      agent = {
        pinentryFlavor = "emacs";
      };
    };
    # jdk = pkgs.jdk.override (args: { ignoreCollisions = true; });
    maven = pkgs.maven.overrideAttrs {
      jdk = pkgs.jdk;
    };
    zsh = pkgs.zsh.overrideAttrs {
      enable = true;
      ohMyZsh = {
        enable = true;
        customPkgs = with pkgs; [
          noreallyjustfuckingstopalready
          zsh-git-prompt
        ];
        plugins = [
          "nix"
        ];
      };
    };
  };

  # What you want.
  # users.users.${config.settings.username}.shell = pkgs.zsh;
  # What you get.
  users.users.logan.shell = pkgs.zsh;

  # let customOhMyZsh = oh-my-zsh: with oh-my-zsh {
  #   enable = true;
  #   customPkgs = with pkgs; [
  #     noreallyjustfuckingstopalready
  #     zsh-git-prompt
  #   ];
  #   plugins = [
  #     "nix"
  #   ];
  #   theme = "robbyrussell";
  # };
  # namei = singleBinary "namei" {
  #   linux = pkgs.glibc.bin;
  #   darwin = pkgs.darwin.system_cmds;
  # };
  packageOverrides = pkgs: with pkgs; {
    # The pkgs part here is important, even though pkgs is in the lexical scope.
    # At the moment this is broken due to "perl532" being missing. Strangely
    # omitting the overlays declaration here makes the issue go away. The
    # problem manifests even if overlays is empty.
    # pkgs.overlays = [
      # Could be inline like this:
      #
      # (self: super: {
      #     freecad = super.callPackage ~/dev/nixpkgs/pkgs/applications/graphics/freecad/default.nix {};
      # })
      # import ./nix/overlays.nix
    # ];

# This records as a flat dependency just called 'shell-packages'. I need to find
# a way to unpack this.
    shellPackages = pkgs.buildEnv {
      extraOutputsToInstall = [ "man" "doc" ];
# "my-packages" isn't very helpful. We should make this profile "default" or
# "shell".
      name = "shell-packages";
      paths = [
        # A grep-sed like alternative. Offers a scripting language for
        # transformations.
        ack
        # Compile natively to tiny devices.
        # Actually I'm not sure what to use. Arduino has no Darwin/macOS version
        # and avr-gcc no longer exists. I can't even find a ticket on it.
        # Perhaps I should file one?
        # arduino
        # avr-gcc
        # This is a suite of GNU utilities.
        coreutils
        # curl does http requests. Comes with MacOS but no reason to use a dated
        # version.
        curl
        # A specialized charting tool using a declarative language. Supports a
        # specific set of charts but I don't remember which. Used by plantuml.
        ditaa
        # The ultimate editor. But how to make it conditional on OS?
        # emacs
        emacsMacport
        # This is a binary apparently. Universal editor settings. Kind of like a
        # pre-prettier. I don't know the nix pacakge though, if it exists.
        # editorconfig
        # Searches for files. Used by projectile in Emacs.
        fd
        # Convert media (sound, video).
        ffmpeg
        # Rasterized image manipulation.
        gimp
        # git manages my code. Comes with MacOS but no reason to use a dated
        # version.
        git
        # Haskell docs say this is necessary for Haskell. See
        # https://docs.haskellstack.org/en/v0.1.10.1/nix_integration.html#using-a-custom-shell-nix-file
        # See Haskell entry for why I'm not bothering.
        # glpk
        # Who wants BSD grep when you could have GNU grep?
        gnugrep
        # Make pretty graphs from a declarative language.
        gnuplot
        # A password storage system.
        gnupg
        # Who wants BSD sed when you could have GNU sed?
        gnused
        # Who wants BSD ls when you could have GNU ls? And other GNU things.
        gnutls
        # A specialized charting tool using a declarative language. Supports a
        # large set of charts. Used by plantuml.
        graphviz

        # Haskell's install information is hard to get a hold of or seems to
        # assume an existing Stack installation. None of these seem appealing to
        # me nor in the spirit of Nix's declarative package management. For now
        # Haskell can take a back seat since I don't use it much anyways.
        # haskell.packages.lts-3_13.ghc
        # An HTML validation tool.
        html-tidy
        # A more visual version of top.
        htop
        # For previewing LaTeX in Emacs.
        imagemagick
        # A spell checker.
        ispell
        # Email gathering and sending. Works with mu.
        isync
        # I think I have some extra memory. Java should take care of that.
        # For now this seems broken - I suspect it is a collision with maven.
        # See https://github.com/NixOS/nixpkgs/issues/56549 - my local version
        # also seems to be Zulu, for what it's worth.
        # jdk
        # Managed Java on a per-project basis. Doesn't exist in nix?
        # jenv
        # JSON parsing, querying, and updating.
        jq
        # For rendering a number of diagrams and documents.
        #latex
        # Give us man pages for GNU stuff.
        manpages
        # Java builds. Pom.xml files as far as the eyes can see.
        maven
        # A program to allow communicating over serial. Perhaps I can script it?
        minicom
        # A union of ping and traceroute - ping all hosts along a route.
        mtr
        # Email indexing, viewing, etc.
        mu
        # Connect to MongoDB for poking around the document store.
        # Currently broken due to gperftools.
        # mongodb
        # This lets me search for parts of nix to help debug collisions.
        nix-index
        # Recursively walks up the file hiearchy to show permissions. Quite
        # helpful! Currently not available as a nix package. Research on this
        # has led to attempting unixtools and nettools. The unixtools package
        # doesn't contain namei, and I could not confirm nettools due to an
        # issue with openssl being out of date in that package.
        # namei
        # A tool for mapping network ports.
        nmap
        # The code injector bootstrap script is written in Node.js, so we need
        # to run it from the command line.
        nodejs
        # Used to do split tunneled VPN connections from the command line. You
        # can also, you know, download it. Looking at you, AnyConnect.
        openconnect
        # oq is like jq but for xml. It parses xml and yaml files and can
        # convert them to xml, yaml, or json (with jq as a backend). It uses the
        # same snytax as jq with the exception of the -i and -o arguments to
        # indicate which format is to be used. It just uses jq internally for
        # all translations.
        #
        # Broken at the moment. Need to file bug.
        # oq
        # Translate human documents from one format to another.
        pandoc
        # Doesn't exist. It's a pip.
        # percol

        # Because sometimes you need something better than grep.
        perl
        # Haskell docs say this is necessary for Haskell. See
        # https://docs.haskellstack.org/en/v0.1.10.1/nix_integration.html#using-a-custom-shell-nix-file
        # See Haskell entry for why I'm not bothering.
        # pcre
        # An interface for reading passwords.
        pinentry
        # A charting tool with a declarative language. Uses graphviz dot, ditaa,
        # and others.
        plantuml
        # Query an HTML DOM from the command line.
        pup
        # Perhaps under another name?
        # pinentry-mac
        # (python2.withPackages (ps: [
          # Disabled until allowBroken is addressed.
          # See https://github.com/mooz/percol/issues/110
          #
          # ps.percol
        # ]))
        # Like Ruby, but not.
        (python3.withPackages (ps: [
        ]))
        # Doesn't exist?
        # rbenv
        # Really fast grep alternative.
        ripgrep
        # A version management tool for Rust.
        rustup
        # A lightweight SQL database which requires no server. This also
        # installs CLI tools in which to access SQLite databases.
        sqlite
        # Highly controllable terminal emulation and session management.
        tmux
        # The penultimate editor.
        vim
        # Renders HTML formatted emails.
        w3m
        # A handy alternative to curl, best suited for downloading content.
        wget
        # Folks suggest installing an application from the AppStore. I'd prefer
        # to have an unattended install of the Wireguard client.
        wireguard-tools
        # A tool for installing node packages. Better than npm in most ways.
        yarn
        # See also oh-my-zsh.
        zsh
        zsh-syntax-highlighting
        # Used by Emacs' Tramp to handle compression over SSH.
        zstd
      ];
      pathsToLink = [ "/Applications" "/bin" "/etc" "/share" ];
      # Get man pages synced.
      postBuild = ''
        if [ -x $out/bin/install-info -a -w $out/share/info ]; then
          shopt -s nullglob
          for i in $out/share/info/*.info $out/share/info/*.info.gz; do
              $out/bin/install-info $i $out/share/info/dir
          done
        fi
      '';
    };
  };
}
