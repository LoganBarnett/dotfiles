#! /usr/bin/env bash

set -e

echo "Installing Emacs (emacs-plus)"
# brew tap d12frosted/emacs-plus
brew tap railwaycat/emacsmacport
echo "emacs tapped"
# "already installed" should not appear if emacs was not upgraded, so the
# check we have should be ok.
# brew install emacs-plus \
#      --with-cocoa \
#      --with-gnutls \
#      --with-librsvg \
#      --with-imagemagick \
#      --with-spacemacs-icon 2>&1 | grep "just not linked" || \
#     brew upgrade emacs-plus \
#          --with-cocoa \
#          --with-gnutls \
#          --with-librsvg \
#          --with-imagemagick \
#          --with-spacemacs-icon 2>&1 | grep "already installed"
./install-package.sh emacs-mac
echo "emacs-plus installed"
# brew link --overwrite emacs-plus
echo "linked emacs-plus"

echo "installing spacemacs"
# git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d || true
git clone https://github.com/syl20bnr/spacemacs ~/.spacemacs.d || true

mkdir -p ~/.vanilla-emacs.d

# I want to maintain a vanilla Emacs configuration alongside Spacemacs.
ln -snf ~/.emacs.d ~/.vanilla-emacs.d

echo "linking elisp dir"
rm ~/.spacemacs.d/private/local/dotfiles || true
ln -s -n -f $PWD/lisp ~/.spacemacs.d/private/local/dotfiles
ln -snf $PWD/lisp/vanilla-init.el ~/.vanilla-emacs.d/init.el
rm -rf ~/.spacemacs.d/private/snippets
ln -s -n -f $PWD/yasnippets ~/.spacemacs.d/private/snippets
# ln -s -n -f $PWD/emacs-config.org ~/.emacs.d/emacs-config.org
echo "This directory managed by dotfiles" > ~/.spacemacs.d/private/local/dotfiles/README.org

echo "Setting up additional layers"

mkdir -p ~/.spacemacs.d/private/layers
cd ~/.spacemacs.d/private/layers

# Deft is for searching - I use it for my org-mode files.
ln -snf ~/Dropbox/notes ~/.deft

# we can preload the packages with this:
# emacs --batch --load=~/.emacs.d/init.el
# OR:
# emacs --batch --load=~/.spacemacs

echo "emacs installation complete!"
