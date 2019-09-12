#! /usr/bin/env bash

set -e

echo "Installing Emacs (emacs-mac)"
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
# ./install-package.sh emacs-mac

brew install emacs-mac \
  --with-glib \
  --with-spacemacs-icon \
  --with-modules \
  --with-xml2 \
  --with-imagemagick || \
  brew upgrade emacs-mac \
    --with-glib \
    --with-spacemacs-icon \
    --with-modules \
    --with-xml2 \
    --with-imagemagick
echo "emacs installed"
# brew link --overwrite emacs-plus
brew link --overwrite emacs-mac
echo "linked emacs"

echo "installing spacemacs"
# git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d || true
git clone https://github.com/syl20bnr/spacemacs ~/.spacemacs.d || true

mkdir -p ~/.vanilla-emacs.d

# I want to maintain a vanilla Emacs configuration alongside Spacemacs.
# This is not fully baked. Spacemacs does not like seeing itself in a symlink,
# and will add a lot of startup cost trying to deal with "duplicate packages".
#
# ln -snf ~/.vanilla-emacs.d ~/.emacs.d
# ln -snf ~/.spacemacs.d ~/.emacs.d
ln -snf ~/.emacs.d ~/.spacemacs.d

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

# we can preload the packages with this:
# emacs --batch --load=~/.emacs.d/init.el
# OR:
# emacs --batch --load=~/.spacemacs

echo "emacs installation complete!"
