#!/usr/bin/env bash
set -e

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
rbenv
ripgrep
sqlite
terraform
thefuck
tmux
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
    #
    # PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
    ln -s -f /usr/local/opt/gnu-sed/libexec/gnubin/sed /usr/local/bin/gsed

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
        APTS="
        ack
        avr-gcc
        awscli
        cargo
        coreutils
        curl
        dos2unix
        elm
        emacs
        ffmpeg
        gnu-sed
        gnuplot
        gnutls
        go
        graphviz
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
        python3-dev
        python3-pip
        ripgrep
        sqlite
        terraform
        thefuck
        tmux
        vim
        w3m
        wget
        yarn
        zsh
        zsh-syntax-highlighting
        "
        sudo apt install -y -qq $APTS
    else
        # we must be in some redhat based distro
        echo "yum not supported yet!"
    fi

    # thefuck is installed via pip on linux
    sudo -H pip install thefuck

    # ripgrep is installed via cargo on Linux (despite the claims of Debian
    # support in the docs - I think it only applies to 18.x and not higher).
    cargo install ripgrep
else
    echo "skipping brew install - not on a supported OS."
fi

echo "installing pup for html parsing with css selectors"
go get github.com/ericchiang/pup

echo "installing misc python packages..."

pip install upsidedown # Needed for tflip.
pip3 install xdice # xdice requires python3.

# TODO: vpn-slice doesn't install from pip3 cleanly. I haven't made a bug yet
# but I should do so soon when I have more time. To fix it, I've added vpn-slice
# as a submodule.
#
# pip3 install vpn-slice # vpn-slice is needed for openconnect DNS issues.

git submodule update
cd vpn-slice
python3 setup.py install
cd -

# pip3 doesn't have a PATH location, or xdice doesn't provide a binary (the docs
# suggest that pip3 is at fault though). Either way we're left fixing it
# manually for now.
ln -f -s /usr/local/lib/python3.7/site-packages/xdice.py /usr/local/bin/xdice
chmod +x /usr/local/lib/python3.7/site-packages/xdice.py

./youtube-dl-install.sh
