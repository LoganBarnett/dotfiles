#! /usr/bin/env bash

echo "[CONFIG] Installing private settings..."

if [[ -d ../dotfiles-private/.git ]]; then
  echo "The dotfiles-private repository is already cloned. Skipping clone."
else
  git clone \
      git@bitbucket.org:LoganBarnett/dotfiles-private.git \
      ../dotfiles-private

fi

# Use this file to populate private settings files (not to be checked into the
# public dotfiles repo). This file itself should have _nothing_ sensitive in it.

# TODO: Check for the files needed and report what needs to be populated.

# See mbsyncrc for usage of this file.
PRIV_DIR=$PWD/../dotfiles-private
ln -F -n -s $PRIV_DIR/email-creds.priv.txt ~/.email-creds.txt

# The .mailrc lets me have email aliases. I don't want to share emails with the
# public though.
ln -F -n -s $PRIV_DIR/mailrc ~/.mailrc
# Floobits is great!
ln -F -n -s $PRIV_DIR/floorc.json ~/.floorc.json

echo "[CONFIG] Installing private settings... Done!"
