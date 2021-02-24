#!/usr/bin/env bash

set -e

# By default git uses vi, which isn't always what we want. Set it to vim
# instead. See:
# http://www.freyskeyd.fr/fixing-there-was-a-problem-with-the-editor-vi-for-git/
#
# This is now set directly in the gitconfig, but left for reference.
# git config --global core.editor $(which vim)


./link-dotfile.sh gitconfig
