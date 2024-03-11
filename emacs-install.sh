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
  #git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.doom-emacs.d || true
  # ln -snf ~/.doom-emacs.d ~/.emacs.d
  git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs || true
  # Clean up old directories that don't get used anymore, to support older
  # configurations.
  rm -rf ~/.doom.d || true
  doom_dir="$HOME/.config/doom"
  rm -rf ~/.config/doom
  mkdir -p ~/.config/doom
  ln -sfn $PWD/doom/init.el $doom_dir/init.el
  ln -sfn $PWD/doom/config.el $doom_dir/config.el
  ln -sfn $PWD/doom/packages.el $doom_dir/packages.el
  ln -sfn $PWD/doom/snippets $doom_dir/snippets
  ln -sfn ~/.config/emacs ~/.emacs.d
  # Remove all prior packages - just make sure we're at a clean slate.  We have
  # to know the Emacs version in advance potentially, but here we just remove
  # all versions.
  rm -rf $HOME/.config/emacs/.local/straight/build-* || true
  rm -rf $HOME/.config/emacs/.local/straight/repos || true
  # But that's not enough to be clean.  Per
  # https://github.com/doomemacs/doomemacs/issues/2802#issuecomment-606654151
  # we need to update MELPA and clean Straight's cache.  But these don't seem to
  # work.  Left for reference.
  # git -C $HOME/.emacs.d/.local/straight/repos/melpa pull
  # git -C $HOME/.emacs.d/.local/straight/repos/org-elpa pull
  # git -C $HOME/.emacs.d/.local/straight/repos/nongnu-elpa pull
  # git -C $HOME/.emacs.d/.local/straight/repos/el-get pull
  # git -C $HOME/.emacs.d/.local/straight/repos/emacsmirror-mirror pull
  rm -f ~/.emacs.d/.local/straight/build-cache.el
  # Use --no-hooks to prevent git hooks from being installed in my home
  # directory. This is not the intention of these hooks and this arg is provided
  # as a release valve for the issue here:
  # https://github.com/doomemacs/doomemacs/issues/5878
  # Use --env to keep it from prompting if we want an env file (I think we do).
  # Use --no-config because we laid the configuration down already.  Otherwise
  # Doom will silently overwrite this.
  ~/.config/emacs/bin/doom install --no-hooks --env --no-config
  # I shouldn't need these init files to be stored locally, since they can be
  # modified upstream and break a reinstall.
  #ln -sfn $PWD/emacs.d/early-init.el ~/.doom-emacs.d/early-init.el
  #ln -sfn $PWD/emacs.d/init.el ~/.doom-emacs.d/init.el
  # If `doom sync` is not run afterwards, Doom is not really loaded - it's a
  # wierd semi-state.  You can't use standard Doom bindings, even though the
  # Doom splash screen comes up.
  doom sync
elif [[ "$distro" == 'vanilla' ]]; then
  mkdir -p ~/.vanilla-emacs.d
  ln -snf $PWD/lisp/vanilla-init.el ~/.vanilla-emacs.d/init.el
fi

# we can preload the packages with this:
# emacs --batch --load=~/.emacs.d/init.el
# OR:
# emacs --batch --load=~/.spacemacs

slog "Emacs installation complete!"
