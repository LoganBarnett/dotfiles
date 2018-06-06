#!/usr/bin/env bash
set -e

# Needed for avr-gcc
brew tap osx-cross/avr

# TODO: Install rvm and source it.
# TODO: Lock postgres to 9.5 until I can upgrade the DB.
# This looks promising: https://gist.github.com/giannisp/ebaca117ac9e44231421f04e7796d5ca
if [ $(uname) = 'Darwin' ]; then

    which brew || \
        /usr/bin/ruby -e \
            "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    xcode-select --install || true
    # ispell so flyspell works on emacs
    BREWS="
ag
ack
avr-gcc
awscli
cask
coreutils
dos2unix
elm
ffmpeg
gnuplot
gnutls
graphviz
ispell
jq
markdown
maven
mongodb
mtr
nmap
node
npm
ocaml
opam
pinentry-mac
plantuml
postgresql
stack
terraform
thefuck
vim
wget
yarn
zsh-syntax-highlighting
"

    echo "updating homebrew"
    brew update

    # Java needs to come first.
    echo "installing Java first..."
    brew cask install java
    echo "installing brews"
    for brew in $BREWS
    do
      ./install-package.sh $brew
    done

    ./install-gpg.sh

    # sed is special
    echo "installing sed"
    brew install gnu-sed --with-default-names || brew upgrade gnu-sed --with-default-names

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
    echo "skipping brew install - not on a supported OS."
fi

echo "installing pup for html parsing with css selectors"
go get github.com/ericchiang/pup
