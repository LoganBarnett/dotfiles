# zsh is managed differently by home-manager and therefore not something we
# can easily make into an overlay. We could break this out into a function and
# pull it in elsewhere though.
{pkgs}: {
  enable = true;
  enableAutosuggestions = true;
  enableSyntaxHighlighting = true;
  oh-my-zsh = {
    enable = true;
    # TODO: These packages and plugins aren't out of reach necessarily but
    # they require some additional work. See
    # https://github.com/nix-community/home-manager/blob/master/modules/programs/zsh.nix
    # for this module so I know what structures the home-manager-zsh setup
    # expects.
    #customPkgs = [
    #  #pkgs.noreallyjustfuckingstopalready
    #  pkgs.zsh-git-prompt
    #];
    plugins = [
      #"nix"
    ];
  };
  loginExtra = ''
      source ~/.zshenv-customized
    '';
  initExtra = ''
      source ${pkgs.zsh-git-prompt}/share/zsh-git-prompt/zshrc.sh
      source ~/.zshrc-customized
    '';
}
