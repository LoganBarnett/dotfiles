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
    # Override tea to build from PR #897 branch which fixes TTY prompting in
    # non-interactive environments.
    (pkgs.tea.overrideAttrs (oldAttrs: {
      src = pkgs.fetchgit {
        url = "https://gitea.com/gitea/tea";
        rev = "6c5811e4e9241b16376beb8eb9138d7951d6090f";
        hash = "sha256-JCvwvVxJGSzr+L+OMfXIeeYsKi+m0PSS83BJ97U4u0c=";
      };
      # Set vendorHash to fetch fresh dependencies instead of using
      # the out-of-sync vendor directory in the source.
      vendorHash = "sha256-+s2uF8xfEoy+0fReRs7ftNFeQklNUr9AeFa9VKjaKas=";
      # Disable tests since one tries to create config file in sandbox.
      doCheck = false;
    }))
    # A spell checker.
    pkgs.ispell
    # IBM font that looks good outside of source code contexts.
    pkgs.ibm-plex
    # A good mono-spaced font that is largely sans-serif, but uses serifs to
    # disambuguate.
    pkgs.source-code-pro
  ];
}
