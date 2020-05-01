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
distro='doom'
# distro='spacemacs'
# distro='vanilla'

if [[ "$distro" == 'spacemacs' ]]; then
  # I want to maintain a vanilla Emacs configuration alongside Spacemacs.
  # This is not fully baked. Spacemacs does not like seeing itself in a symlink,
  # and will add a lot of startup cost trying to deal with "duplicate packages".
  #
  git clone https://github.com/syl20bnr/spacemacs ~/.spacemacs.d || true
  ln -snf ~/.spacemacs.d ~/.emacs.d
  echo "linking elisp dir"
  rm ~/.spacemacs.d/private/local/dotfiles || true
  ln -s -n -f $PWD/lisp ~/.spacemacs.d/private/local/dotfiles
  rm -rf ~/.spacemacs.d/private/snippets
  ln -s -n -f $PWD/yasnippets ~/.spacemacs.d/private/snippets
  # ln -s -n -f $PWD/emacs-config.org ~/.emacs.d/emacs-config.org
  echo "This directory managed by dotfiles" > ~/.spacemacs.d/private/local/dotfiles/README.org
  echo "Setting up additional layers"
  mkdir -p ~/.spacemacs.d/private/layers
  cd ~/.spacemacs.d/private/layers
elif [[ "$distro" == 'doom' ]]; then
  git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.doom-emacs.d || true
  ln -snf ~/.doom-emacs.d ~/.emacs.d
  ~/.doom-emacs.d/bin/doom install
  ln -sfn $PWD/emacs.d/early-init.el ~/.doom-emacs.d/early-init.el
  ln -sfn $PWD/emacs.d/init.el ~/.doom-emacs.d/init.el
  ln -sfn $PWD/doom/init.el ~/.doom.d/init.el
  ln -sfn $PWD/doom/config.el ~/.doom.d/config.el
  ln -sfn $PWD/doom/packages.el ~/.doom.d/packages.el
elif [[ "$distro" == 'vanilla' ]]; then
  mkdir -p ~/.vanilla-emacs.d
  ln -snf $PWD/lisp/vanilla-init.el ~/.vanilla-emacs.d/init.el
fi

# we can preload the packages with this:
# emacs --batch --load=~/.emacs.d/init.el
# OR:
# emacs --batch --load=~/.spacemacs

echo "emacs installation complete!"
