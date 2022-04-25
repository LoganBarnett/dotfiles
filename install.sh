#!/usr/bin/env bash

set -euo pipefail

source bash-logging

dir="$(dirname "${BASH_SOURCE[0]}")"
source $dir/dotfiles-functions.sh

# TODO: add various application settings not living under ~
# TODO: add iStatsMenu
# TODO: add Slack settings

# Link dotfiles before installing nix and home-manager, so home-manager can
# find the right files during setup.
slog "Linking easy dotfiles"
./link-dotfiles.sh
slog "Done linking easy dotfiles"

slog "Installing nix..."
./nix-install.sh
slog "Done installing nix."

slog "Doing OS specific installation..."
if [ $(uname) != 'Darwin' ]; then
  apt update
else
  $dir/darwin-install.sh
fi

slog "Creating ssh key if it doesn't exist"
./create-ssh-key.sh
slog "Done create ssh key"

slog "Configuring git"
./git-config.sh

# get submodules set up
git submodule init || true
git submodule update || true

# link harder things
mkdir -p ~/.config

ln -snf $PWD/bin ~/bin

slog "Installing shell settings."
./install-shell.sh

slog "Installing Homebrew (for casks and troublesome nix packages)."
./brew-install.sh

slog "Installing packages..."
./install-packages.sh

slog "Installing Rust..."
./install-rust.sh

slog "installing node modules"
# TODO: Move necessary node modules into nix so we needn't worry about what node
# version we're using in that moment.
#./install-node-modules.sh

slog "installing GPG..."
#./gpg-install.sh

#./daw-install.sh

# TODO: Does the nix package let me _run_ mongodb?
#./mongodb-install.sh

# TODO: Maybe change some of these from "install" to "configure".
slog "Setting up Emacs..."
#./emacs-install.sh

if [ $(uname) = 'Darwin' ]; then
  # alfred workflows
  #cd $start_dir
  ./install-workflows.sh
else
  slog "skipping alfred install - not on osx"
fi

# TODO: See if enough of this is installed in nix.
#slog "Installing (some) Java support..."
#./install-java.sh

if [[ "$USER" == "logan.barnett" ]]; then
  slog "Installing puppet for work..."
  # TODO: See if nix can do this.
  #./puppet-install.sh
fi

slog "Installing GNUPG settings..."
./gpg-install.sh

slog "Writing out private settings..."
./install-private.sh

slog "Installing email support..."
./install-email.sh

slog "Installing Firefox settings..."
./firefox-install.sh

slog 'Done! All installation is successful!'
