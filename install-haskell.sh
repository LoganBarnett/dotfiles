#!/usr/bin/env bash

set -e

# Package exists for homebrew but not aptitude.
if [ $(uname) = 'Darwin' ]; then
  ./install-package.sh stack
else
  curl -sSL https://get.haskellstack.org/ | sh
fi

stack upgrade
