#!/usr/bin/env bash

source nix/bash-logging

dir="$(dirname "${BASH_SOURCE[0]}")"

# Make sure we actually fail here. Homebrew changes things up frequently by my
# standards, up to even rewriting their install scripts.
set -euo pipefail

if [ $(uname) = 'Darwin' ]; then
  cd $dir
  slog "Installing homebrew..."
  # Redirect stdin from /dev/null to put the script into a non-interactive
  # mode.
  /bin/bash -e \
    "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)" \
    < /dev/null
  slog "Installing casks..."
  $dir/brew-cask-install.sh
else
  slog "Skipping cask installs - not osx."
fi
