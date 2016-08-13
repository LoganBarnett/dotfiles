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

if [ $(uname) = 'Darwin' ]; then

    # ispell so flyspell works on emacs
    BREWS="vim wget node htop nmap cask zsh-syntax-highlighting npm mongodb ispell coreutils mtr gpg nvm graphviz postgresql"

    echo "updating homebrew"
    brew update
    echo "installing brews"
    brew install $BREWS

    # sed is special
    echo "installing sed"
    brew install gnu-sed --with-default-names

    # spacemacs is special
    echo "installing emacs for eventually installing spacemacs"
    brew tap d12frosted/emacs-plus
    brew install emacs-plus --with-cocoa --with-gnutls --with-librsvg --with-imagemagick --with-spacemacs-icon
    brew linkapps

    # java needs a special section because of ordering
    echo "installing java and maven"
    brew cask install java
    brew install maven
elif [ $(uname) = 'Linux' ]; then
    if [ $(which apt-get) != '' ]; then
        echo "installing packages via apt-get"
        # how can you not have curl? ugh
        APTS="curl zsh zsh-syntax-highlighting"
        sudo apt-get install -y -qq $APTS
    else
        # we must be in some redhat based distro
        echo "yum not supported yet!"
    fi
else
    echo "skipping brew install - not on osx"
fi

# install zsh
echo "installing oh-my-zsh"
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
if [ $SHELL != '/bin/zsh' ];
then
    chsh -s /bin/zsh
fi

# node modules
NODE_MODULES="jshint"

npm i -g $NODE_MODULES

#zsh customizations
echo "installing oh my zsh customizations"
if [ $(uname) = 'Darwin' ]; then
    echo "installing osx dns flushing for oh my zsh"
    cd ~/.oh-my-zsh/custom/plugins && git clone git@github.com:eventi/noreallyjustfuckingstopalready.git
else
    echo "skipping osx dns flushing for oh my zsh - not osx"
fi

if [ $(uname) = 'Darwin' ]; then
    cd $start_dir
    ./install-casks.sh
else
    echo "skipping cask installs - not osx"
fi

echo "installing spacemacs"
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d || true

# TODO: add a way of reading in the paradox-github token or generating it if it doesn't exist

echo "installing rvm"
\curl -sSL https://get.rvm.io | bash -s stable

echo "sourcing rvm"
if [ $(uname) = 'Darwin' ]; then
    source /Users/logan/.rvm/scripts/rvm
else
    echo "skipping rvm source - not on OSX"
    echo "you know, you really should add support for other envs for rvm..."
fi

echo "installing gems"
GEMS="heroku jekyll"
gem install $GEMS

if [ $(uname) = 'Darwin' ]; then
    # alfred workflows
    cd $start_dir
    ./install-workflows.sh
else
    echo "skipping alfred install - not on osx"
fi

echo "all installation is successful"
