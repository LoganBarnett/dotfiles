#! /usr/bin/env bash

# Install, but do not link.
./install-package.sh python3

# These executables are (semi)suffixed already.
for b in 'python3' 'pip3' 'python3-config'
do
  ln -sfn $(brew --prefix python3)/bin/$b /usr/local/bin/$b
done
