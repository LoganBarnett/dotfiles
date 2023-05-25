#!/usr/bin/env bash

####
# This is a recovery script for when nix goes belly up. It's really hard to
# recover a system dependent upon nix when a reinstall is needed. This script
# should be idempotent in terms of its only job is to leave us with a clean,
# working nix installation once it's all done. This differs from the intent of
# nix-install.sh in that the reinstall process is lengthy, and doesn't have a
# focus on upgrading packages.
####

source bash-logging

slog "The first step is to make nix think we are worthy of install (no existing
nix). We will remove everything! This script requires sudoer."

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

slog "Purged all nix files."

slog "Restoring pre-nix system wide shell configs..."
sudo mv /etc/bashrc{.backup-before-nix,}
sudo mv /etc/zshrc{.backup-before-nix,}

slog "At this point we should be free to run the install logic - delegating to
nix-install.sh"

./nix-install.sh
