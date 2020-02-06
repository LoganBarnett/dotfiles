#! /usr/bin/env bash

pushd ..
git clone git@bitbucket.org:LoganBarnett/dotfiles-private.git || true
popd

# Use this file to populate private settings files (not to be checked into the
# public dotfiles repo). This file itself should have _nothing_ sensitive in it.

# TODO: Check for the files needed and report what needs to be populated.

# See mbsyncrc for usage of this file.
PRIV_DIR=$PWD/../dotfiles-private
ln -F -n -s $PRIV_DIR/email-creds.priv.txt ~/.email-creds.txt