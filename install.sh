#!/usr/bin/env bash

set -e

start_dir=$PWD

# TODO: add various application settings not living under ~
# TODO: add iStatsMenu
# TODO: add Slack settings
# TODO: add ssh key generation
# TODO: add key pair
# TODO: install istatmenu

if [ $(uname) != 'Darwin' ]; then
  apt update
fi

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
ln -snf $PWD/awesome ~/.config/awesome

ln -snf $PWD/bin ~/bin

# Installing Haskell must become before the shell since some plugins require
# Haskell and Stack are installed first.
echo "Installing Haskell..."
./install-haskell.sh

./install-shell.sh

if [ $(uname) = 'Darwin' ]; then
    cd $start_dir
    echo "installing homebrew..."
    # Redirect stdin from /dev/null to put the script into a non-interactive
    # mode.
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" < /dev/null
    echo "installing casks"
    ./install-casks.sh
else
    echo "skipping cask installs - not osx"
fi

echo "Installing packages..."
./install-packages.sh

echo "Installing HTML tools..."
./html-tools-install.sh

echo "Installing Haskell..."
./install-haskell.sh

echo "Installing Rust..."
./install-rust.sh

echo "Installing htop..."
./install-htop.sh

echo "Installing oq..."
./oq-install.sh

echo "Installing mysql (tools?)..."
./install-mysql.sh

# node modules
echo "installing node modules"
./install-node-modules.sh

echo "settting up ocaml's opam"
./install-opam.sh

echo "Installing python3..."
./python-install.sh

echo "installing GPG..."
./install-gpg.sh

./daw-install.sh

./install-latex.sh

# nix must come after gpg since it uses gpg.
./install-nix.sh

./mongodb-install.sh

echo "setting up spacemacs"
./emacs-install.sh

# TODO: add a way of reading in the paradox-github token or generating it if it
# doesn't exist

# echo "installing rvm"
# We need to add the key from Michal Papis so we can verify RVM is the real
# deal. This is found in the installation instructions. This command fails
# though. I'm not sure why. "no route to host"
# gpg --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3
# \curl -sSL https://get.rvm.io | bash -s stable

echo "sourcing rvm"
if [ $(uname) = 'Darwin' ]; then
    # source $HOME/.rvm/scripts/rvm
    echo "skipping rvm sourcing until we can install it reliably"
else
    echo "skipping rvm source - not on OSX"
    echo "you know, you really should add support for other envs for rvm..."
fi

./install-rubygems.sh
if [ $(uname) = 'Darwin' ]; then
    # alfred workflows
    cd $start_dir
    ./install-workflows.sh
else
    echo "skipping alfred install - not on osx"
fi

echo "Installing (some) Java support..."
./install-java.sh

if [[ "$USER" == "logan.barnett" ]]; then
    echo "Installing puppet for work..."
    ./puppet-install.sh
fi

echo "Installing diagramming support..."
./install-diagram.sh

echo "Installing email support..."
./install-email.sh

echo "writing out private settings"
./install-private.sh

../dotfiles-private/private-install.sh

echo "all installation is successful"
