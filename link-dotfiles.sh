#!/usr/bin/env bash

set -e

PWD=$(pwd)
EASYFILES="
Xdefaults
bash_profile
bashrc
pentadactylrc
profile
vimrc
zlogin-customized
zshenv-customized
zshrc-customized
"

# link easy things
for dotfile in $EASYFILES
do
    ./link-dotfile.sh $dotfile
done
