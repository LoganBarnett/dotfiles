#! /usr/bin/env bash

set -e

echo "installing spacemacs"
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d || true

echo "linking elisp dir"
rm ~/.emacs.d/private/local/dotfiles
ln -s -n $PWD/lisp ~/.emacs.d/private/local/dotfiles

echo "This directory managed by dotfiles" >> ~/.emacs.d/private/local/dotfiles/README.org

# we can preload the packages with this:
# emacs --batch --load=~/.emacs.d/init.el
# OR:
emacs --batch --load=~/.spacemacs

echo "emacs installation complete!"
