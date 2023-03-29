#!/usr/bin/env bash

set -euo pipefail

source bash-logging
slog "Installing private settings..."

if [[ -d ../dotfiles-private/.git ]]; then
  slog "The dotfiles-private repository is already cloned. Skipping clone."
else
  git clone \
      git@bitbucket.org:LoganBarnett/dotfiles-private.git \
      ../dotfiles-private

fi

# The .mailrc lets me have email aliases. I don't want to share emails with the
# public though.
ln -fns $PRIV_DIR/mailrc ~/.mailrc
# Floobits is great!
ln -fns $PRIV_DIR/floorc.json ~/.floorc.json

cd $PRIV_DIR
slog "Installing private settings from private repo..."
./install.sh
slog "Installing private settings... Done!"
