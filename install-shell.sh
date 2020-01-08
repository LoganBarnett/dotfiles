#! /usr/bin/env bash

# install zsh
echo "installing oh-my-zsh"
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
zshPath=$(which zsh)
if [ $SHELL != "$zshPath" ];
then
    if ! grep "$zshPath" /etc/shells ; then
        echo "Need sudoer for adding $zshPath to /etc/shells..."
        echo "$zshPath" | sudo tee -a /etc/shells > /dev/null
    fi
    chsh -s $zshPath
fi

if [ $(uname) = 'Linux' ]; then
    # in Ubuntu apt-get does not provide this package, so install manually
    echo "installing zsh syntax highlighting"
    git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
fi

#zsh customizations
echo "installing oh my zsh customizations"
if [ $(uname) = 'Darwin' ]; then
    echo "installing osx dns flushing for oh my zsh ----- ignore the error if it already exists."
    cd ~/.oh-my-zsh/custom/plugins && git clone git@github.com:eventi/noreallyjustfuckingstopalready.git || true
else
    echo "skipping osx dns flushing for oh my zsh - not osx"
fi

# https://github.com/zsh-git-prompt/zsh-git-prompt
cd ~/.oh-my-zsh/custom/plugins && git clone git@github.com:zsh-git-prompt/zsh-git-prompt.git || true
ln -snf ~/.oh-my-zsh/custom/plugins/zsh-git-prompt/{zshrc.sh,zsh-git-prompt.plugin.zsh}
cd ~/.oh-my-zsh/custom/plugins/zsh-git-prompt/haskell/ && stack build && stack install
