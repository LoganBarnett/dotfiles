{ environment, config, pkgs, lib, fetchFromGitHub, ... }:
let
  # PyQt5 = pkgs.callPackage ./PyQt5.nix;
  # PyQt5 = (import ./PyQt5.nix);

  # python39 = pkgs.callPackage ./PyQt5.nix;
  # openconnect-sso-src = builtins.fetchTarball "https://github.com/vlaci/openconnect-sso/archive/master.tar.gz";
  nixpkgs.overlays = [
    # (import ./overlays/crystal.nix)
    (import ./overlays/gnupg.nix)
    (import ./overlays/maven.nix)
    #(import ./overlays/percol.nix)
    (import ./overlays/speedtest-cli.nix)
    (import ./overlays/tmux.nix)
    # (import (builtins.fetchTarball
    #   "https://github.com/oxalica/rust-overlay/archive/master.tar.gz"))
    # (import "${openconnect-sso-src}/overlay.nix")
    # (import ./overlays/openconnect-sso.nix)
    # (import ./openconnect-sso.nix)
    # (import "${builtins.fetchTarball https://github.com/vlaci/openconnect-sso/archive/master.tar.gz}/overlay.nix")
  ];
in
{
  nixpkgs.config = {
    # Some packages are not "free". We need to specifically bless those.
    allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
      "ngrok"
      "unrar"
    ];
    # Somehow this can get lost, and I'm not convinced this is home-managers'
    # nor nix's doing. That said, this setting seems to have no effect.
    networking.hostname = "neon.proton";
    permittedInsecurePackages = [
      "openssl-1.0.2u"
    ];
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
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

  # This gets oh-my-zsh where we can find it.
  home.file.".oh-my-zsh".source = config.lib.file.mkOutOfStoreSymlink "${pkgs.oh-my-zsh.outPath}/share/oh-my-zsh";

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
    ++ (import ./general.nix){pkgs = pkgs;}
    ++ (if builtins.getEnv "USER" == "logan"
         then (import ./personal.nix){pkgs = pkgs;}
         else (import ../../dotfiles-private/work-new-e-ah.nix){pkgs = pkgs;}
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
