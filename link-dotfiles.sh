#! /usr/bin/env bash

set -e

PWD=$(pwd)
EASYFILES="
Xdefaults
avnrc
floorc.json
gitconfig
gitignore_global
ideavim
jsbeautifyrc
npmrc
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
    ./link-dotfile.sh $dotfile
done
