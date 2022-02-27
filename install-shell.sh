#!/usr/bin/env bash

dir="$(dirname "${BASH_SOURCE[0]}")"

source $dir/bash-logging
slog "Installing oh-my-zsh."
# Much of this file might not be necessary anymore, much of zsh (and oh-my-zsh)
# can be controlled via nix.
#
# sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
zshPath=$(which zsh)
if [ $SHELL != "$zshPath" ]; then
  if ! grep "$zshPath" /etc/shells ; then
    slog "Need sudoer for adding $zshPath to /etc/shells..."
    echo "$zshPath" | sudo tee -a /etc/shells > /dev/null
  fi
  # Unfortunately this prompts for a password. Alas.
  chsh -s $zshPath
fi

#zsh customizations
# log "Installing oh my zsh customizations."
# if [ $(uname) = 'Darwin' ]; then
#   log "Installing osx dns flushing for oh my zsh ----- ignore the error if it already exists."
#   cd ~/.oh-my-zsh/custom/plugins && git clone git@github.com:eventi/noreallyjustfuckingstopalready.git || true
# else
#   log "Skipping osx dns flushing for oh my zsh - not osx."
# fi
