#!/usr/bin/env bash
# The nix daemon doesn't work with -u.
set -eo pipefail

source bash-logging

slog "Prompting for sudo..."
sudo printf "\n" # To ensure we're in a sudo context first.
# Remove backups, or the installer will freak out.
sudo rm -rf \
  /etc/zshrc.backup-before-nix \
  /etc/bashrc.backup-before-nix \
  /etc/bash.bashrc.backup-before-nix
# Pipe yes because there's no way to do an officially unattended install. Though
# piping anything to it removes the tty and then it works unattended...
#
# Even with all of this, the installer will crash and burn and the slightest
# presence of itself. Upgrading is a different command which we should run
# immediately.
#
# Use unencrypted nix store volume as of macOS Catalina.
yes | sh <(curl -L https://nixos.org/nix/install) \
  --daemon \
  || true

# Their generated script checks first, but it better damn sure be there or this
# endeavor is in error.
. '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'

#slog "Attempting to upgrade nix. It may fail but we will proceed."
# Attempt to upgrade.
#sudo nix-channel --update || true
# nix-env -iA nixpkgs.nix
slog "Reloading daemon..."
sudo launchctl remove org.nixos.nix-daemon
sudo launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist

mkdir -p ~/.config/nixpkgs
# Link the file for this machine. This allows for host specific configurations.
machineName=$(scutil --get ComputerName | tr -d $'\n' || hostname)
slog "Found machine name of '$machineName'."
# This machine has not been named yet.
if [[ "$machineName" =~ 'Mac' ]] ; then
  slog "Give this machine a proper name:"
  read machineName
  # There's a bunch of different ways to name the machine across platforms. I
  # have a script to make it portable.
  bin/hostname-set "$machineName"
fi
mkdir -p ~/.config/home-manager
ln -snf $PWD/nix/home.nix ~/.config/nixpkgs/home.nix
# The home.nix appears to need to go here now.
ln -snf $PWD/nix/home.nix ~/.config/home-manager/home.nix

slog "Loading nix-daemon so we don't need to close our shell..."
. '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
slog "nix-daemon loaded."

./nix-home-manager-install.sh

# The guide recommends the below flag for catalina.
# https://nixos.org/manual/nix/stable/#chap-quick-start
# --darwin-use-unencrypted-nix-store-volume

slog "Trusting system store..."
./nix-trust-system-store.sh
