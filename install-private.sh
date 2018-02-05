#! /usr/bin/env bash

# Use this file to populate private settings files (not to be checked into the
# public dotfiles repo). This file itself should have _nothing_ sensitive in it.

# TODO: Check for the files needed and report what needs to be populated.

# See mbsyncrc for usage of this file.
ln -F -n -s $PWD/email-creds.priv.txt ~/.email-creds.txt
