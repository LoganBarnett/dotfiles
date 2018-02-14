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
    echo "installing homebrew..."
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    echo "installing casks"
    ./install-casks.sh
else
    echo "skipping cask installs - not osx"
fi

# node modules
./install-node-modules.sh

echo "settting up ocaml's opam"
# I'm guessing that -y will accept the shell integration question. Perhaps it
# should be -n and we just check in the configured settings.
opam init -y --comp 4.03.0
eval `opam config env`
echo "getting other ocaml dev tools"
opam update
# Contributing to Flow was the primary motivator here.
opam install -y ocamlfind sedlex js_of_ocaml

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

echo "Installing email support..."
./install-email.sh

echo "writing out private settings"
./install-private.sh

echo "all installation is successful"
