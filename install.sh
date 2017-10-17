#!/usr/bin/env bash

set -e

start_dir=$PWD

# TODO: add various application settings not living under ~
# TODO: add iStatsMenu
# TODO: add Slack settings
# TODO: add ssh key generation
# TODO: add key pair
# TODO: install istatmenu

echo "creating ssh key if it doesn't exist"
./create-ssh-key.sh
echo "done create ssh key"

echo "linking easy dotfiles"
./link-dotfiles.sh
echo "done linking easy dotfiles"

echo "configuring git"
./config-git.sh

# get submodules set up
git submodule init || true
git submodule update || true

# link harder things
mkdir -p ~/.config
ln -F -s -n $PWD/awesome ~/.config/awesome

ln -F -s -n $PWD/bin ~/bin

if [ $(uname) = 'Darwin' ]; then

    xcode-select --install || true
    # ispell so flyspell works on emacs
    BREWS="
cask
coreutils
dos2unix
elm
ffmpeg
gnupg
gnutls
graphviz
htop
ispell
jq
markdown
mongodb
mtr
nmap
node
npm
nvm
ocaml
opam
pinentry-mac
plantuml
postgresql
stack
thefuck
vim
wget
yarn
zsh-syntax-highlighting
"

    echo "updating homebrew"
    brew update
    echo "installing brews"
    for brew in $BREWS
    do
      brew install $brew || brew upgrade $brew
    done

    ./install-gpg.sh

    # sed is special
    echo "installing sed"
    brew install gnu-sed --with-default-names || brew upgrade gnu-sed --with-default-names

    # spacemacs is special
    echo "installing emacs for eventually installing spacemacs"
    brew tap d12frosted/emacs-plus
    echo "emacs tapped"
    # "already installed" should not appear if emacs was not upgraded, so the
    # check we have should be ok.
    brew install emacs-plus --with-cocoa --with-gnutls --with-librsvg --with-imagemagick --with-spacemacs-icon 2>&1 | grep "just not linked" || brew upgrade emacs-plus --with-cocoa --with-gnutls --with-librsvg --with-imagemagick --with-spacemacs-icon 2>&1 | grep "already installed"
    echo "emacs-plus installed"
    brew link --overwrite emacs-plus
    echo "linked emacs-plus"
    brew linkapps
    echo "linked apps"

    # java needs a special section because of ordering
    echo "installing java and maven"
    brew cask install java
    brew install maven || brew upgrade maven
elif [ $(uname) = 'Linux' ]; then
    if [ $(which apt-get) != '' ]; then
        echo "installing packages via apt-get"
        # how can you not have curl? ugh
        APTS="curl zsh emacs python3-dev python3-pip"
        sudo apt-get install -y -qq $APTS
    else
        # we must be in some redhat based distro
        echo "yum not supported yet!"
    fi

    # thefuck is installed via pip on linux
    sudo -H pip install thefuck

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

if [ $(uname) = 'Linux' ]; then
    # in Ubuntu apt-get does not provide this package, so install manually
    echo "installing zsh syntax highlighting"
    git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
fi

# node modules
./install-node-modules.sh

#zsh customizations
echo "installing oh my zsh customizations"
if [ $(uname) = 'Darwin' ]; then
    echo "installing osx dns flushing for oh my zsh"
    cd ~/.oh-my-zsh/custom/plugins && git clone git@github.com:eventi/noreallyjustfuckingstopalready.git || true
else
    echo "skipping osx dns flushing for oh my zsh - not osx"
fi

if [ $(uname) = 'Darwin' ]; then
    cd $start_dir
    echo "installing casks"
    ./install-casks.sh
else
    echo "skipping cask installs - not osx"
fi

echo "settting up ocaml's opam"
# I'm guessing that -y will accept the shell integration question. Perhaps it
# should be -n and we just check in the configured settings.
opam init -y --comp 4.03.0
eval `opam config env`
echo "getting other ocaml dev tools"
opam update
# Contributing to Flow was the primary motivator here.
opam install -y ocamlfind sedlex js_of_ocaml

# This is really spacemacs installation.
echo "setting up spacemacs"
./emacs-install.sh

# TODO: add a way of reading in the paradox-github token or generating it if it doesn't exist

echo "installing rvm"
\curl -sSL https://get.rvm.io | bash -s stable

echo "sourcing rvm"
if [ $(uname) = 'Darwin' ]; then
    source $HOME/.rvm/scripts/rvm
else
    echo "skipping rvm source - not on OSX"
    echo "you know, you really should add support for other envs for rvm..."
fi

echo "installing gems"
GEMS="heroku jekyll github-pages bundler"
gem install $GEMS

if [ $(uname) = 'Darwin' ]; then
    # alfred workflows
    cd $start_dir
    ./install-workflows.sh
else
    echo "skipping alfred install - not on osx"
fi

echo "all installation is successful"
