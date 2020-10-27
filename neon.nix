{
  programs.gnupg.agent = {
    # The pinentryFlavor seems to have no effect. Perhaps this will change with
    # a patch.
    # pinentryFlavor = "qt";
  };
  # namei = singleBinary "namei" {
  #   linux = pkgs.glibc.bin;
  #   darwin = pkgs.darwin.system_cmds;
  # };
  packageOverrides = pkgs: with pkgs; {
    shellPackages = pkgs.buildEnv {
      extraOutputsToInstall = [ "man" "doc" ];
# "my-packages" isn't very helpful. We should make this profile "default" or
# "shell".
      name = "shell-packages";
      paths = [
        # A grep-sed like alternative. Offers a scripting language for
        # transformations.
        ack
        # Can't find where this gets included yet.
        # avr
        # The manager for Rust and its tooling.
        cargo
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
        # git manages my code. Comes with MacOS but no reason to use a dated
        # version.
        git
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
        # An HTML validation tool.
        html-tidy
        # A more visual version of top.
        htop
        # A spell checker.
        ispell
        # Email gathering and sending. Works with mu.
        isync
        # A transcoding system.
        ffmpeg
        # JSON parsing, querying, and updating.
        jq
        # A union of ping and traceroute - ping all hosts along a route.
        mtr
        # Email indexing, viewing, etc.
        mu
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
        oq
        # Translate human documents from one format to another.
        pandoc
        # Doesn't exist. It's a pip.
        # percol

        # An interface for reading passwords.
        pinentry
        # A charting tool with a declarative language. Uses graphviz dot, ditaa,
        # and others.
        plantuml
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
        # A tool for installing node packages. Better than npm in most ways.
        yarn
        zsh-syntax-highlighting
      ];
      pathsToLink = [ "/Applications" "/bin" "/etc" "/share" ];
    };
  };
}
