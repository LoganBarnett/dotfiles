{
  packageOverrides = pkgs: with pkgs; {
    shellPackages = pkgs.buildEnv {
      extraOutputsToInstall = [ "man" "doc" ];
      name = "shell-packages";
      paths = [
        # A grep-sed like alternative. Offers a scripting language for
        # transformations.
        ack
        # The manager for Rust and its tooling.
        cargo
        # This is a suite of GNU utilities.
        coreutils
        # curl does http requests. Comes with MacOS but no reason to use a dated
        # version.
        curl
        # A specialized charting tool using a declarative language. Supports a
        # specific set of charts but I don't remember which.
        ditaa
        # Searches for files. Used by projectile in Emacs.
        fd
        # git manages my code. Comes with MacOS but no reason to use a dated
        # version.
        git
        # Who wants BSD grep when you could have GNU grep?
        gnugrep
        # Make pretty graphs from a declarative language.
        gnuplot
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
        # A transcoding system.
        ffmpeg
        # JSON parsing, querying, and updating.
        jq
        # A union of ping and traceroute - ping all hosts along a route.
        mtr
        # A tool for mapping network ports.
        nmap
        # oq is like jq but for xml. It parses xml and yaml files and can
        # convert them to xml, yaml, or json (with jq as a backend). It uses the
        # same snytax as jq with the exception of the -i and -o arguments to
        # indicate which format is to be used. It just uses jq internally for
        # all translations.
        oq
        # Translate human documents from one format to another.
        pandoc
        # An interface for reading passwords.
        pinentry
        # A charting tool with a declarative language. Uses graphviz dot, ditaa,
        # and others.
        plantuml
        # Like Ruby, but not.
        (python3.withPackages (ps: [
        ]))
        # Really fast grep alternative.
        ripgrep
        # A lightweight SQL database which requires no server. This also
        # installs CLI tools in which to access SQLite databases.
        sqlite
        # Highly controllable terminal emulation and session management.
        tmux
        # The penultimate editor.
        vim
        # A handy alternative to curl, best suited for downloading content.
        wget
        # A tool for installing node packages. Better than npm in most ways.
        yarn
      ];
      pathsToLink = [ "/Applications" "/share" "/bin" ];
    };
  };
}
