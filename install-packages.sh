#!/usr/bin/env bash
set -e

if [ $(uname) = 'Darwin' ]; then

    which brew || \
        /usr/bin/ruby -e \
            "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    xcode-select --install || true
    # ispell so flyspell works on emacs
    BREWS="
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
java
jq
markdown
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
    echo "installing brews"
    for brew in $BREWS
    do
      ./install-package.sh $brew
    done

    ./install-gpg.sh

    # sed is special
    echo "installing sed"
    brew install gnu-sed --with-default-names || brew upgrade gnu-sed --with-default-names

    brew linkapps
    echo "linked apps"

    # maven needs a special section because of ordering
    echo "installing java and maven"
    ./install-package.sh maven
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
