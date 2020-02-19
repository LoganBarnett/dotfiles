#! /usr/bin/env bash

set -e

PWD=$(pwd)
EASYFILES="
Xdefaults
avnrc
floorc.json
ideavim
jsbeautifyrc
npmrc
pentadactylrc
spacemacs
tmux.conf
urxvt
vimrc
yarnrc
zshenv
zshrc
"

# link easy things
for dotfile in $EASYFILES
do
    ./link-dotfile.sh $dotfile
done
