#!/usr/bin/env bash

start_dir=$PWD

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
BREWS="vim wget node htop nmap cask zsh-syntax-highlighting npm mongodb ispell coreutils mtr gpg nvm graphviz postgresql"

echo "updating homebrew"
brew update
echo "installing brews"
brew install $BREWS

# node modules
NODE_MODULES="jshint"

npm i -g $NODE_MODULES

#zsh customizations
echo "installing oh my zsh customizations"
cd ~/.oh-my-zsh/custom/plugins && git clone git@github.com:eventi/noreallyjustfuckingstopalready.git

cd $start_dir
./install-casks.sh

# spacemacs
echo "ensuring spacemacs setup"
brew tap d12frosted/emacs-plus
brew install emacs-plus --with-cocoa --with-gnutls --with-librsvg --with-imagemagick --with-spacemacs-icon
brew linkapps

git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d || true

# java needs a special section because of ordering
echo "installing java and maven"
brew cask install java
brew install maven

# TODO: add a way of reading in the paradox-github token or generating it if it doesn't exist

echo "making zsh our default shell"
# use zsh as my shell
if [ $SHELL != '/bin/zsh' ];
  then
    chsh -s /bin/zsh 
fi

echo "installing rvm"
\curl -sSL https://get.rvm.io | bash -s stable

echo "sourcing rvm"
source /Users/logan/.rvm/scripts/rvm

echo "installing gems"
GEMS="heroku jekyll"
gem install $GEMS

# alfred workflows
cd $start_dir
./install-workflows.sh

echo "all installation is successful"
