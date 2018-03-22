#! /usr/bin/env bash

set -e

echo "Installing Emacs (emacs-plus)"
brew tap d12frosted/emacs-plus
echo "emacs tapped"
# "already installed" should not appear if emacs was not upgraded, so the
# check we have should be ok.
brew install emacs-plus --with-cocoa --with-gnutls --with-librsvg --with-imagemagick --with-spacemacs-icon 2>&1 | grep "just not linked" || brew upgrade emacs-plus --with-cocoa --with-gnutls --with-librsvg --with-imagemagick --with-spacemacs-icon 2>&1 | grep "already installed"
echo "emacs-plus installed"
brew link --overwrite emacs-plus
echo "linked emacs-plus"

echo "installing spacemacs"
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d || true

echo "linking elisp dir"
rm ~/.emacs.d/private/local/dotfiles || true
ln -s -n -f $PWD/lisp ~/.emacs.d/private/local/dotfiles
rm -rf ~/.emacs.d/private/snippets
ln -s -n -f $PWD/yasnippets ~/.emacs.d/private/snippets
# ln -s -n -f $PWD/emacs-config.org ~/.emacs.d/emacs-config.org
echo "This directory managed by dotfiles" > ~/.emacs.d/private/local/dotfiles/README.org

echo "Setting up additional layers"

mkdir -p ~/.emacs.d/private/layers
cd ~/.emacs.d/private/layers

# Deft is for searching - I use it for my org-mode files.
ln -snf ~/Dropbox/notes ~/.deft

echo "Setting up notmuch layer"
git clone https://github.com/cmiles74/spacemacs-notmuch-layer.git notmuch || true

# we can preload the packages with this:
# emacs --batch --load=~/.emacs.d/init.el
# OR:
emacs --batch --load=~/.spacemacs

echo "emacs installation complete!"
