#!/usr/bin/env bash
set -euo pipefail

sudo printf "\n" # To ensure we're in a sudo context first.
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
  --darwin-use-unencrypted-nix-store-volume \
  || true

# Their generated script checks first, but it better damn sure be there or this
# endeavor is in error.
. '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'

#echo "[DOTFILES] Attempting to upgrade nix. It may fail but we will proceed."
# Attempt to upgrade.
#sudo nix-channel --update || true
# nix-env -iA nixpkgs.nix
echo "[DOTFILES] Reloading daemon..."
sudo launchctl remove org.nixos.nix-daemon
sudo launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist

mkdir -p ~/.config/nixpkgs
# Link the file for this machine. This allows for host specific configurations.
machineName=$(scutil --get ComputerName | cut -d' ' -f1 | tr -d $'\n' || hostname)
ln -snf $PWD/$machineName.nix ~/.config/nixpkgs/config.nix

echo "[DOTFILES] Loading nix-daemon so we don't need to close our shell..."
. '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
echo "[DOTFILES] nix-daemon loaded."

echo "[DOTFILES] Installing nix packages for this host..."
nix-env -iA nixpkgs.shellPackages

# The guide recommends the below flag for catalina.
# https://nixos.org/manual/nix/stable/#chap-quick-start
# --darwin-use-unencrypted-nix-store-volume

echo "[DOTFILES] Trusting system store..."
./nix-trust-system-store.sh
