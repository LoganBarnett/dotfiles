{
  home-manager,
  config,
  pkgs,
  lib,
  fetchFromGitHub,
  ...
}:
let
  # PyQt5 = pkgs.callPackage ./PyQt5.nix;
  # PyQt5 = (import ./PyQt5.nix);

  # python39 = pkgs.callPackage ./PyQt5.nix;
  # openconnect-sso-src = builtins.fetchTarball "https://github.com/vlaci/openconnect-sso/archive/master.tar.gz";
in
{
  imports = [
    ./home-modules/aider-chat.nix
    ./home-configs/gh-cli.nix
  ];
  # Let Home Manager install and manage itself.
  # programs.home-manager.enable = true;
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
  # Enabling flakes is implicit now, so don't even enable this, despite advice
  # to the contrary still floating around.
  # programs.direnv.nix-direnv.enableFlakes = true;
  # Start keychain, which ensures only one ssh-agent and one gpg-agent. The
  # results print out the standard environment variables much like ssh-agent and
  # gpg-agent do, so we must eval them to take effect.  Weird things happen if
  # that is omitted, since it will use the system defaults (which are wrong or
  # separate).
  #
  # Without --noask, the SSH_AGENT_PID won't be setup.  Somehow though,
  # identities can be added and used without issue, except via Emacs (and
  # perhaps other tools).  I still have to add the SSH keys by hand with a
  # simple `ssh-add` invocation, but it is a one-time thing.
  programs.keychain = {
    enable = true;
    agents = [ "gpg" "ssh" ];
    extraFlags = [ "--noask" "--quiet" ];
    enableZshIntegration = true;
  };
  programs.tmux = import ./tmux.nix;
  programs.zsh = import ./zsh.nix { pkgs = pkgs; };

  # This is the magic that makes fonts get installed to ~/Library/Fonts on
  # macOS, and I imagine other dirs for other systems.
  # This was causing problems after the nix-darwin migration but I don't recall
  # specifically what it was.  nix-darwin manages fonts now.
  # fonts.fontconfig.enable = true;

  home.enableNixpkgsReleaseCheck = (!(
    config.home.version.full == "25.05"
    && (lib.strings.hasPrefix "25.11" lib.version)
  ));

  home.activation = {
    # Is there a better way to handle the directory? Relative dir does not work.
    ispell-config = ''
      ln -snf $(realpath ~/dev/dotfiles/ispell_english) ~/.ispell_english
    '';
  };

  # In order to actually use some of this on my system (a Mac), I need to
  # wire up some of the nix-managed directories to the real thing. One example
  # is mapping ~/.nix-profile/Library/Fonts to ~/Library/Fonts, so macOS will
  # detect the added fonts. We want the same for ~/Applications, and perhaps
  # more.
  #
  # NOTE: Both Spotlight and Alfred made the unforuntate choice not to follow
  # symlinks. This means the applications linked here will never work. However,
  # this is what the runner (https://github.com/LoganBarnett/runner) tool aims
  # to solve.
  # Much like how fonts can manage the ~/Library/Fonts, perhaps there is another
  # home-manager feature for applications?
  home.file."Applications/home-manager".source = let
  apps = pkgs.buildEnv {
    name = "home-manager-applications";
    paths = config.home.packages;
    pathsToLink = "/Applications";
  };
  in lib.mkIf pkgs.stdenv.targetPlatform.isDarwin "${apps}/Applications";
  # My little colorized/bracketed logger.
  home.file.".bash-logging".source = ./bash-logging;

  # This gets oh-my-zsh where we can find it.
  home.file.".oh-my-zsh".source = config.lib.file.mkOutOfStoreSymlink
    "${pkgs.oh-my-zsh.outPath}/share/oh-my-zsh";

  # For Emacs to prettify JavaScript files, this config must be laid down (or it
  # will not use great defaults).
  home.file.".jsbeautifyrc".source = ./jsbeautifyrc;

  # Disabled due to ChatGPT's separate subscription required for API keys, if I
  # even want to use this library in the first place.
  # home.file.".uniteai.yml".source = ./uniteai.yaml;

  # This puts the file in the store and links it, but we don't want that because
  # we write to this file quite a bit to update entries. A true symlink to the
  # source is desired. Instead this is handled via home-manager's activation
  # block.
  # home.file.".ispell_english".source = config.lib.file.mkOutOfStoreSymlink ../ispell_english;

  # Lifted from https://github.com/Yumasi/nixos-home/blob/master/zsh.nix
  # There aren't a lot of examples or documentation for nix, home-manager, etc
  # out there so this is a great example to see how we can source an entire
  # directory. I haven't tested it yet.
  #  home.file.".config/zsh/scripts".source = ./files/scripts;
  #  home.file.".config/zsh/scripts".recursive = true;

  # lorri automatically watches direnv and shell.nix and installs as needed.
  # It only works on Linux though.
  # services.lorri.enable = true;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "24.11";
}
