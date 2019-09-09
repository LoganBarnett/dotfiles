#!/usr/bin/env bash
set -e

# TODO: Install rvm and source it.
# TODO: Lock postgres to 9.5 until I can upgrade the DB.
# This looks promising: https://gist.github.com/giannisp/ebaca117ac9e44231421f04e7796d5ca
if [ $(uname) = 'Darwin' ]; then

    which brew || \
        /usr/bin/ruby -e \
            "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    xcode-select --install || true

    # Needed for avr-gcc
    brew tap osx-cross/avr

    # Upgrade existing packages before installing (potentially) new ones.
    brew outdated | xargs brew reinstall

    # ispell so flyspell works on emacs
    # pandoc allows converting HTML to org syntax.
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
gnu-sed
go
graphviz
grep
ispell
jenv
jq
markdown
maven
mtr
nmap
node
npm
ocaml
opam
pandoc
percol
pinentry-mac
plantuml
postgresql
python3
ripgrep
sqlite
stack
terraform
thefuck
vim
w3m
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
    # notes from sed install:
    # If you really need to use these commands with their normal names, you
    # can add a "gnubin" directory to your PATH from your bashrc like:

    # PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"

    # Some systems have workarounds expecting the original sed to remain in
    # place. This makes the system work for both cases assuming the check is an
    # OS check. It should be assumed that "sed" is the right sed, and not the
    # impostor FreeBSD sed.
    ln -f -s /usr/local/bin/{,g}sed

    ./install-aws-cli-tools.sh

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

echo "installing misc python packages..."

pip install upsidedown # Needed for tflip.
pip3 install xdice # xdice requires python3.
pip3 install vpn-slice # vpn-slice is needed for openconnect DNS issues.

# pip3 doesn't have a PATH location, or xdice doesn't provide a binary (the docs
# suggest that pip3 is at fault though). Either way we're left fixing it
# manually for now.
ln -f -s /usr/local/lib/python3.7/site-packages/xdice.py /usr/local/bin/xdice
chmod +x /usr/local/lib/python3.7/site-packages/xdice.py
