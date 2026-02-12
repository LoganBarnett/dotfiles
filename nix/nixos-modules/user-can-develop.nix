{ username }: { pkgs, ... }: {
  imports = [
    ./git-config.nix
    (import ./gpg-config.nix { gpg-user = username; })
  ];
  environment.systemPackages = [
    # Gives us diff --color support via GNU diff.
    pkgs.diffutils
    # Just loads and unloads environment variables based on directory. Really
    # useful with nix to declare local dependencies for a project, and see them
    # auto loaded when entering the directory. See the local shell configuration
    # for how direnv is hooked up.
    #
    # See more at https://nixos.wiki/wiki/Development_environment_with_nix-shell
    pkgs.direnv
    # git manages my code. Comes with MacOS but no reason to use a dated
    # version.
    pkgs.git
    # CLI tool for interacting with Gitea servers.
    pkgs.tea
    # A spell checker.
    pkgs.ispell
    # IBM font that looks good outside of source code contexts.
    pkgs.ibm-plex
    # A good mono-spaced font that is largely sans-serif, but uses serifs to
    # disambuguate.
    pkgs.source-code-pro
  ];
}
