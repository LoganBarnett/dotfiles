#!/usr/bin/env bash

set -e

source bash-logging

slog "Setting up Emacs distro - Emacs installed via nix-pkg..."
distro='doom'
slog "Installing Emacs distro $distro..."
# distro='spacemacs'
# distro='vanilla'

if [[ "$distro" == 'spacemacs' ]]; then
  # I want to maintain a vanilla Emacs configuration alongside Spacemacs.
  # This is not fully baked. Spacemacs does not like seeing itself in a symlink,
  # and will add a lot of startup cost trying to deal with "duplicate packages".
  #
  git clone https://github.com/syl20bnr/spacemacs ~/.spacemacs.d || true
  ln -snf ~/.spacemacs.d ~/.emacs.d
  slog "linking elisp dir"
  rm ~/.spacemacs.d/private/local/dotfiles || true
  ln -s -n -f $PWD/lisp ~/.spacemacs.d/private/local/dotfiles
  rm -rf ~/.spacemacs.d/private/snippets
  ln -s -n -f $PWD/yasnippets ~/.spacemacs.d/private/snippets
  # ln -s -n -f $PWD/emacs-config.org ~/.emacs.d/emacs-config.org
  slog "This directory managed by dotfiles" > ~/.spacemacs.d/private/local/dotfiles/README.org
  slog "Setting up additional layers"
  mkdir -p ~/.spacemacs.d/private/layers
  cd ~/.spacemacs.d/private/layers
elif [[ "$distro" == 'doom' ]]; then
  git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.doom-emacs.d || true
  ln -snf ~/.doom-emacs.d ~/.emacs.d
  ~/.doom-emacs.d/bin/doom install
  # I shouldn't need these init files to be stored locally, since they can be
  # modified upstream and break a reinstall.
  #ln -sfn $PWD/emacs.d/early-init.el ~/.doom-emacs.d/early-init.el
  #ln -sfn $PWD/emacs.d/init.el ~/.doom-emacs.d/init.el
  ln -sfn $PWD/doom/init.el ~/.doom.d/init.el
  ln -sfn $PWD/doom/config.el ~/.doom.d/config.el
  ln -sfn $PWD/doom/packages.el ~/.doom.d/packages.el
  mkdir -p $PWD/doom/snippets
  ln -sfn $PWD/doom/snippets ~/.doom.d/snippets
elif [[ "$distro" == 'vanilla' ]]; then
  mkdir -p ~/.vanilla-emacs.d
  ln -snf $PWD/lisp/vanilla-init.el ~/.vanilla-emacs.d/init.el
fi

# we can preload the packages with this:
# emacs --batch --load=~/.emacs.d/init.el
# OR:
# emacs --batch --load=~/.spacemacs

slog "Emacs installation complete!"
