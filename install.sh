#!/usr/bin/env bash

set -euo pipefail

dir="$(dirname "${BASH_SOURCE[0]}")"
source $dir/dotfiles-functions.sh

# TODO: add various application settings not living under ~
# TODO: add iStatsMenu
# TODO: add Slack settings

log "Installing nix..."
./nix-install.sh
log "Done installing nix."

log "Doing OS specific installation..."
if [ $(uname) != 'Darwin' ]; then
  apt update
else
  $dir/darwin-install.sh
fi

log "Creating ssh key if it doesn't exist"
./create-ssh-key.sh
log "Done create ssh key"

log "Linking easy dotfiles"
./link-dotfiles.sh
log "Done linking easy dotfiles"

log "Configuring git"
./git-config.sh

# get submodules set up
git submodule init || true
git submodule update || true

# link harder things
mkdir -p ~/.config
ln -snf $PWD/awesome ~/.config/awesome

ln -snf $PWD/bin ~/bin

log "Installing shell settings."
./install-shell.sh

if [ $(uname) = 'Darwin' ]; then
  cd $dir
  log "Installing homebrew..."
  # Redirect stdin from /dev/null to put the script into a non-interactive
  # mode.
  /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" < /dev/null
  log "Installing casks..."
  $dir/brew-cask-install.sh
else
  log "Skipping cask installs - not osx."
fi

log "Installing packages..."
./install-packages.sh

log "Installing Haskell..."
./install-haskell.sh

log "Installing Rust..."
./install-rust.sh

log "Installing mysql (tools?)..."
./install-mysql.sh

# node modules
log "installing node modules"
./install-node-modules.sh

log "Installing python3..."
./python-install.sh

log "installing GPG..."
./gpg-install.sh

./daw-install.sh

./install-latex.sh

./mongodb-install.sh

log "Setting up Emacs."
./emacs-install.sh

# TODO: add a way of reading in the paradox-github token or generating it if it
# doesn't exist

./install-rubygems.sh
if [ $(uname) = 'Darwin' ]; then
  # alfred workflows
  cd $start_dir
  ./install-workflows.sh
else
  log "skipping alfred install - not on osx"
fi

log "Installing (some) Java support..."
./install-java.sh

if [[ "$USER" == "logan.barnett" ]]; then
  log "Installing puppet for work..."
  ./puppet-install.sh
fi

log "Installing diagramming support..."
./install-diagram.sh

log "Installing email support..."
./install-email.sh

log "Installing gnu utils..."
./gnu-install.sh

log "writing out private settings"
./install-private.sh

../dotfiles-private/private-install.sh

log "all installation is successful"
