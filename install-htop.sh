#! /usr/bin/env bash

set -e

./install-package.sh htop

mkdir -p ~/.config/htop
ln -snf ~/dev/dotfiles/htoprc ~/.config/htop/htoprc
