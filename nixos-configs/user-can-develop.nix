{
  flake-inputs,
  pkgs,
  system,
  ...
}:
let
  emacs-unfreeze = pkgs.callPackage ../derivations/emacs-unfreeze.nix { };
  flac2alac = pkgs.callPackage ../derivations/flac2alac.nix { };
  gif2mp4 = pkgs.callPackage ../derivations/gif2mp4.nix { };
  gpg-dev-signing-key-setup =
    pkgs.callPackage ../derivations/gpg-dev-signing-key-setup.nix
      { };
  git-outstanding-repos =
    pkgs.callPackage ../derivations/git-outstanding-repos.nix
      { };
  host-wait = pkgs.callPackage ../derivations/host-wait.nix { };
  image-create = pkgs.callPackage ../derivations/image-create.nix { };
  nix-direnv-add-envrc =
    pkgs.callPackage ../derivations/nix-direnv-add-envrc.nix
      { };
  nix-host-key-install =
    pkgs.callPackage ../derivations/nix-host-key-install.nix
      { };
  nix-host-new = pkgs.callPackage ../derivations/nix-host-new.nix { };
  remote-deploy = pkgs.callPackage ../derivations/remote-deploy.nix { };
  rpi-host-new = pkgs.callPackage ../derivations/rpi-host-new.nix {
    inherit image-create;
  };
  secret-server-token-get =
    pkgs.callPackage ../derivations/secret-server-token-get.nix
      { };
  webp2png = pkgs.callPackage ../derivations/webp2png.nix { };
in
{
  imports = [
    ../nixos-modules/git-config.nix
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
    # A command runner / task tool that reads a Justfile in the project root.
    pkgs.just
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
    # Check whether each NixOS/nix-darwin host's active generation matches
    # what the flake would deploy.  Useful for rolling out changes piecemeal
    # and confirming every host has applied the latest config.
    flake-inputs.flake-sync-status.packages.${system}.default
    # Send SIGUSR2 to a frozen Emacs process to unfreeze it.
    emacs-unfreeze
    # Batch-convert FLAC audio files to ALAC/M4A.
    flac2alac
    # Convert an animated GIF to an MP4.
    gif2mp4
    # Create or recreate the GPG signing subkey for headless development hosts.
    gpg-dev-signing-key-setup
    # Report git repositories with unstaged changes or unpushed commits.
    git-outstanding-repos
    # Poll a host with ping until it responds.
    host-wait
    # Build a Raspberry Pi disk image for a named host.
    image-create
    # Initialize a nix-direnv .envrc and flake.nix in a new project.
    nix-direnv-add-envrc
    # Install the host SSH key from the secrets store via rage.
    nix-host-key-install
    # Checklist entrypoint for creating a new Nix-managed host.
    nix-host-new
    # Copy the flake to a remote host and run nixos-rebuild switch.
    remote-deploy
    # Generate keys and build a disk image for a new Raspberry Pi host.
    rpi-host-new
    # Obtain an OAuth2 bearer token from a Secret Server instance.
    secret-server-token-get
    # Convert a WebP image to PNG.
    webp2png
  ];
}
