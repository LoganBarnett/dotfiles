{ environment, config, pkgs, lib, fetchFromGitHub, ... }:
let
  # PyQt5 = pkgs.callPackage ./PyQt5.nix;
  # PyQt5 = (import ./PyQt5.nix);

  # python39 = pkgs.callPackage ./PyQt5.nix;
  # openconnect-sso-src = builtins.fetchTarball "https://github.com/vlaci/openconnect-sso/archive/master.tar.gz";
  nixpkgs.overlays = [
    (import ./overlays/cacert.nix)
    # (import ./overlays/crystal.nix)
    (import ./overlays/gnupg.nix)
    (import ./overlays/maven.nix)
    #(import ./overlays/percol.nix)
    (import ./overlays/speedtest-cli.nix)
    (import ./overlays/tmux.nix)
    # Give us rust-docs.
    (import ./overlays/rust.nix)
    # (import (builtins.fetchTarball
    #   "https://github.com/oxalica/rust-overlay/archive/master.tar.gz"))
    # (import "${openconnect-sso-src}/overlay.nix")
    # (import ./overlays/openconnect-sso.nix)
    # (import ./openconnect-sso.nix)
    # (import "${builtins.fetchTarball https://github.com/vlaci/openconnect-sso/archive/master.tar.gz}/overlay.nix")
  ];
in
{
  nixpkgs.config = (import ./general-config.nix) {
      lib = lib;
      pkgs = pkgs;
    }
    // (if builtins.getEnv "USER" == "logan"
         then (import ./personal-config.nix) {
           lib = lib;
           pkgs = pkgs;
         }
         else (import ../../dotfiles-private/work-new-e-ah-config.nix) {
           lib = lib;
           pkgs = pkgs;
         }
       )
      ;

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
  # Start keychain, which ensures only one ssh-agent and one gpg-agent. This may
  # prompt us for a password though. The results print out the standard
  # environment variables much like ssh-agent and gpg-agent do, so we must eval
  # them to take effect.  Weird things happen if that is omitted, since it will
  # use the system defaults (which are wrong or separate).
  programs.keychain = {
    enable = true;
    agents = [ "gpg" "ssh" ];
    extraFlags = [ "--quiet" ];
    enableZshIntegration = true;
  };
  programs.tmux = import ./tmux.nix;
  programs.zsh = import ./zsh.nix { pkgs = pkgs; };

  # This is the magic that makes fonts get installed to ~/Library/Fonts on
  # macOS, and I imagine other dirs for other systems.
  fonts.fontconfig.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage. But really I'll only ever be running this from the
  # user I want to apply the nix config to, so just use the variables we already
  # have. It also works for situations where I can't control the username (such
  # as on a work machine).
  home.username = builtins.getEnv "USER";
  home.homeDirectory = builtins.getEnv "HOME";

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
  home.file.".bash-logging".source = ../bash-logging;

  # This gets oh-my-zsh where we can find it.
  home.file.".oh-my-zsh".source = config.lib.file.mkOutOfStoreSymlink "${pkgs.oh-my-zsh.outPath}/share/oh-my-zsh";

  # For Emacs to prettify JavaScript files, this config must be laid down (or it
  # will not use great defaults).
  home.file.".jsbeautifyrc".source = ../jsbeautifyrc;

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

  home.packages = []
    ++ (import ./general-packages.nix){pkgs = pkgs;}
    ++ (if builtins.getEnv "USER" == "logan"
         then (import ./personal-packages.nix){pkgs = pkgs;}
         else (import ../../dotfiles-private/work-new-e-ah-packages.nix) {
           pkgs = pkgs;
         }
       )
  ;
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
