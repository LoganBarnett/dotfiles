#!/usr/bin/env bash

source bash-logging

dir="$(dirname "${BASH_SOURCE[0]}")"

if [ $(uname) = 'Darwin' ]; then
  cd $dir
  slog "Installing homebrew..."
  # Redirect stdin from /dev/null to put the script into a non-interactive
  # mode.
  /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" < /dev/null
  slog "Installing casks..."
  export PATH="$PATH:/opt/homebrew/bin"
  $dir/brew-cask-install.sh
else
  slog "Skipping cask installs - not osx."
fi
