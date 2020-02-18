#! /usr/bin/env bash

# This script moves a dotfile from this dir to ~/.dotfile.

# grep used to suppress warning of symlink existing
WARNING="File exists"
ln -s -n $PWD/$1 ~/.$1 2>&1 | grep -v "$WARNING" || true
