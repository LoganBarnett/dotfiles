#! /usr/bin/env bash

PWD=$(pwd)
EASYFILES="
floorc.json
oh-my-zsh
zshrc
vimrc
pentadactylrc
Xdefaults
tmux.conf
urxvt
ideavim
gitconfig
spacemacs
zshenv
avnrc"

# link easy things
for dotfile in $EASYFILES
do
    # grep used to suppress warning of symlink existing
    WARNING="ln: $HOME/.$dotfile: File exists"
    ln -s -n $PWD/$dotfile ~/.$dotfile 2>&1 | grep -v "$WARNING"
done
