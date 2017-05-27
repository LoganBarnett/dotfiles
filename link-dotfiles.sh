#! /usr/bin/env bash

PWD=$(pwd)
EASYFILES="
Xdefaults
avnrc
floorc.json
gitconfig
ideavim
oh-my-zsh
pentadactylrc
spacemacs
tmux.conf
urxvt
vimrc
zshenv
zshrc
"

# link easy things
for dotfile in $EASYFILES
do
    # grep used to suppress warning of symlink existing
    WARNING="ln: $HOME/.$dotfile: File exists"
    ln -s -n $PWD/$dotfile ~/.$dotfile 2>&1 | grep -v "$WARNING"
done
