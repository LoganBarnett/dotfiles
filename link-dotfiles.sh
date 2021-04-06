#! /usr/bin/env bash

set -e

PWD=$(pwd)
EASYFILES="
Xdefaults
bash-logging
bash_profile
bashrc
floorc.json
gemrc
ideavim
ispell_english
jsbeautifyrc
npmrc
pentadactylrc
profile
spacemacs
tmux.conf
urxvt
vimrc
yarnrc
zlogin
zshenv
zshrc
"

# link easy things
for dotfile in $EASYFILES
do
    ./link-dotfile.sh $dotfile
done
