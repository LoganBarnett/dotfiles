#!/usr/bin/env bash

####
# This is a recovery script for when nix goes belly up. It's really hard to
# recover a system dependent upon nix when a reinstall is needed. This script
# should be idempotent in terms of its only job is to leave us with a clean,
# working nix installation once it's all done. This differs from the intent of
# nix-install.sh in that the reinstall process is lengthy, and doesn't have a
# focus on upgrading packages.
####

source nix/bash-logging

slog "The first step is to make nix think we are worthy of install (no existing
nix). We will remove everything! This script requires sudoer."

slog "Disabling the nix-daemon..."

sudo launchctl disable system/org.nixos.nix-daemon

slog "Hold onto your butts - no turning back now..."

sudo rm -rf \
  /etc/nix \
  /nix \
  /var/root/.nix-profile \
  /var/root/.nix-defexpr \
  /var/root/.nix-channels \
  $HOME/.nix-profile \
  $HOME/.nix-defexpr \
  $HOME/.nix-channels \
  $HOME/.config/home-manager
# If reinstalling from an existing home-manager installation, you'll probably
# have previous generations floating around somewhere.  This cleans that up, per
# the installation error that pops up from home-manager if this doesn't happen.
rm $HOME/.local/state/nix/profiles/home-manager*
rm $HOME/.local/state/home-manager/gcroots/current-home
slog "Purged all nix files."

slog "Restoring pre-nix system wide shell configs..."
sudo mv /etc/bashrc{.backup-before-nix,}
sudo mv /etc/zshrc{.backup-before-nix,}

slog "At this point we should be free to run the install logic - delegating to
nix-install.sh"

./nix-install.sh
