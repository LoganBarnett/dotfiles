#!/usr/bin/env bash 

# TODO: add various application settings not living under ~
# TODO: add iStatsMenu
# TODO: add Slack settings
# TODO: add ssh key generation
# TODO: add key pair
# TODO: install istatmenu

./create-ssh-key.sh

PWD=$(pwd)
EASYFILES="oh-my-zsh zshrc vimrc pentadactylrc Xdefaults tmux.conf urxvt ideavim gitconfig spacemacs"

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
ln -s -h $PWD/awesome ~/.config/awesome

ln -s -h $PWD/bin ~/bin

# ispell so flyspell works on emacs
BREWS="vim wget node htop nmap cask zsh-syntax-highlighting npm mongodb ispell coreutils"

brew update
brew install $BREWS

CASKS="slack chromium discord skype"

brew cask install $CASKS

# spacemacs
echo "ensuring spacemacs setup"
brew tap d12frosted/emacs-plus
brew install emacs-plus --with-cocoa --with-gnutls --with-librsvg --with-imagemagick --with-spacemacs-icon
brew linkapps

git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d || true

# TODO: add a way of reading in the paradox-github token or generating it if it doesn't exist

echo "making zsh our default shell"
# use zsh as my shell
if [ $SHELL != '/bin/zsh' ];
  then
    chsh -s /bin/zsh 
fi

echo "all installation is successful"
