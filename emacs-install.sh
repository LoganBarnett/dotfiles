#! /usr/bin/env bash

set -e

echo "installing spacemacs"
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d || true

echo "linking elisp dir"
rm ~/.emacs.d/private/local/dotfiles || true
ln -s -n -f $PWD/lisp ~/.emacs.d/private/local/dotfiles
rm -rf ~/.emacs.d/private/snippets
ln -s -n -f $PWD/yasnippets ~/.emacs.d/private/snippets
ln -s -n -f $PWD/emacs-config.org ~/.emacs.d/emacs-config.org

echo "This directory managed by dotfiles" >> ~/.emacs.d/private/local/dotfiles/README.org

# we can preload the packages with this:
# emacs --batch --load=~/.emacs.d/init.el
# OR:
emacs --batch --load=~/.spacemacs

echo "emacs installation complete!"
