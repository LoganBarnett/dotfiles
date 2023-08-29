# zsh is managed differently by home-manager and therefore not something we
# can easily make into an overlay. We could break this out into a function and
# pull it in elsewhere though.
{pkgs}: {
  enable = true;
  enableAutosuggestions = true;
  syntaxHighlighting = {
    enable = true;
  };
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
    theme = "robbyrussell";
  };
  loginExtra = ''
      source ~/.zshenv-customized
    '';
  initExtra = ''
      source ${pkgs.zsh-git-prompt}/share/zsh-git-prompt/zshrc.sh
      source ~/.zshrc-customized
    '';
  shellAliases = {
    # -G is BSD, but with Nix we use --color now.
    ls="ls -aG --color=auto";
    grep="grep --color=auto";
    b="bundle exec";
    rlb="RUBYLIB=lib bundle exec";
    curl-json="curl -v -H 'Accept: application/json'";

    # git
    gs="git status";
    gfp="git push --force-with-lease";
    grc="git rebase --continue";
    gro="git restore --ours";
    grt="git restore --theirs";
    glp="git log --pretty=format:'%Cred%h%Creset %<(60,trunc)%s %Cgreen%<(12,trunc)%cr %C(bold blue)%<(12,trunc)%an%Creset %C(yellow)%<(20,mtrunc)%d%Creset' --abbrev-commit";
    gbu="git branch --set-upstream-to=origin/$(git branch --show-current) $(git branch --show-current)";

    # ripgrep
    rgh="rg --hidden --glob '!.git'"; # Search hidden files.

    # yarn flow management because it happens a lot.
    yf="yarn flow";
    yfs="yarn flow stop";
    yfr="yfs && yf";
  };
}
