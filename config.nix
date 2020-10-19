{
  programs.gnupg.agent = {
    pinentryFlavor = "qt";
  };
  packageOverrides = pkgs: with pkgs; {
    myPackages = pkgs.buildEnv {
# "my-packages" isn't very helpful. We should make this profile "default" or
# "shell".
      name = "my-packages";
      paths = [
        ack
        # Can't find where this gets included yet.
        # avr
        cargo
        coreutils
        # curl does http requests. Comes with MacOS but no reason to use a dated
        # version.
        curl
        ditaa
        emacs
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
        gnuplot
        gnupg
        # Who wants BSD sed when you could have GNU sed?
        gnused
        # Who wants BSD ls when you could have GNU ls? And other GNU things.
        gnutls
        graphviz
        html-tidy
        htop
        ispell
        isync
        ffmpeg
        jq
        mtr
        mu
        nmap
        nodejs
        # oq is like jq but for xml. It parses xml and yaml files and can
        # convert them to xml, yaml, or json (with jq as a backend). It uses the
        # same snytax as jq with the exception of the -i and -o arguments to
        # indicate which format is to be used. It just uses jq internally for
        # all translations.
        oq
        pandoc
        # Doesn't exist. It's a pip.
        # percol
        pinentry
        # pinentry-mac
        plantuml
        # Perhaps under another name?
        # pinentry-mac
        # (python2.withPackages (ps: [
          # Disabled until allowBroken is addressed.
          # See https://github.com/mooz/percol/issues/110
          #
          # ps.percol
        # ]))
        (python3.withPackages (ps: [
        ]))
        # Doesn't exist?
        # rbenv
        ripgrep
        sqlite
        tmux
        vim
        w3m
        wget
        yarn
        zsh-syntax-highlighting
      ];
      pathsToLink = [ "/Applications" "/share" "/bin" ];
    };
  };
}
