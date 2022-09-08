#! /usr/bin/env bash

# This script moves a dotfile from this dir to ~/.dotfile.

# grep used to suppress warning of symlink existing
WARNING="File exists"
for private in "$@" ; do
  ln -s -n -f $(realpath $private) ~/.$(basename $private) 2>&1 \
    | grep -v "$WARNING" || true
done
