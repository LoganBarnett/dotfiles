#!/usr/bin/env bash
# The nix daemon doesn't work with -u.
set -eo pipefail

source nix/bash-logging

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

slog "Reloading daemon..."
sudo launchctl remove org.nixos.nix-daemon
# nix-env -iA nixpkgs.nix
# The -w is necessary to fix an issue if the nix-daemon ever got disabled.  -w
# forces the "Disabled" key to get rewritten. and thus prevents startup issues
# here.
# https://github.com/NixOS/nix/issues/2780
sudo launchctl load -w /Library/LaunchDaemons/org.nixos.nix-daemon.plist
# sudo launchctl enable system/org.nixos.nix-daemon.plist
# Their generated script checks first, but it better damn sure be there or this
# endeavor is in error.
. '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'

mkdir -p ~/.config/nixpkgs
# Link the file for this machine. This allows for host specific configurations.
machineName=$(scutil --get ComputerName | tr -d $'\n' || hostname)
slog "Found machine name of '$machineName'."
# This machine has not been named yet.
if [[ "$machineName" =~ "'s Mac" ]] ; then
  slog "Give this machine a proper name:"
  read machineName
  # There's a bunch of different ways to name the machine across platforms. I
  # have a script to make it portable.
  bin/hostname-set "$machineName"
fi

slog "Loading nix-daemon so we don't need to close our shell..."
. '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
slog "nix-daemon loaded."

# This indicates issues that can come up with a user missing. This can come up
# when on an extended-priviledge account (no sudo, but separate account for it).
# https://github.com/nix-community/home-manager/issues/3734
# Example commands to fix:
# sudo mkdir /nix/var/nix/profiles/per-user/$user
# chown -R $user:nixbld /nix/var/nix/profiles/per-user/$user

./nix-home-manager-install.sh

# The guide recommends the below flag for catalina.
# https://nixos.org/manual/nix/stable/#chap-quick-start
# --darwin-use-unencrypted-nix-store-volume

slog "Trusting system store..."
./nix-trust-system-store.sh
