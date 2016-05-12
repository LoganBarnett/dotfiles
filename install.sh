#!/usr/bin/env bash 

# TODO: add various application settings not living under ~
# TODO: add iStatsMenu
# TODO: add Slack settings
# TODO: add ssh key generation
# TODO: add key pair
# TODO: install istatmenu

./create-ssh-key.sh

PWD=$(pwd)
EASYFILES="oh-my-zsh zshrc vimrc pentadactylrc Xdefaults tmux.conf emacs.d urxvt ideavim gitconfig spacemacs"

# get submodules set up
git submodule init
git submodule update 

# link easy things
for dotfile in $EASYFILES
do
    ln -s -n $PWD/$dotfile ~/.$dotfile
done

# link harder things
mkdir -p ~/.config
ln -s $PWD/awesome ~/.config/awesome

BREWS="vim wget node htop nmap cask oh-my-zsh zsh-syntax-highlighting"

brew install $BREWS

CASKS="slack chromium"

brew cask install $CASKS

# spacemacs
brew tap d12frosted/emacs-plus
brew install emacs-plus --with-cocoa --with-gnutls --with-librsvg --with-imagemagick --with-spacemacs-icon
brew linkapps

# TODO: add a way of reading in the paradox-github token or generating it if it doesn't exist

# use zsh as my shell
if [ $SHELL != '/bin/zsh' ];
  then
    chsh -s /bin/zsh 
fi

