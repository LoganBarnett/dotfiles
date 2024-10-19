#!/usr/bin/env bash

####
# This is a recovery script for when nix goes belly up. It's really hard to
# recover a system dependent upon nix when a reinstall is needed. This script
# should be idempotent in terms of its only job is to leave us with a clean,
# working nix installation once it's all done. This differs from the intent of
# nix-install.sh in that the reinstall process is lengthy, and doesn't have a
# focus on upgrading packages.
#
# I have found this is very much a moving target.  Make sure there's nothing
# running from /nix because we need to unmount it.  This will require shutting a
# lot of stuff down, and tearing down any shell tabs open.  You'll need a new
# shell that forcibly uses /bin/zsh as its startup shell, since it's configured
# to use the Nix version already.
#
# Additional tidbits might get updated here:
# https://nix.dev/manual/nix/2.18/installation/uninstall.html#linux
####

source nix/bash-logging

slog "The first step is to make nix think we are worthy of install (no existing
nix). We will remove everything! This script requires sudoer."

slog "Disabling the Linux builder if it's running..."
sudo launchctl bootout system/org.nixos.linux-builder

slog "Disabling the nix-daemon..."

# TODO: Make this a macOS only step.
sudo launchctl bootout system/org.nixos.nix-daemon

slog "Hold onto your butts - no turning back now..."

slog "Removing /nix volume..."
sudo diskutil apfs deleteVolume /nix
# These might be needed, but maybe not.
# sudo diskutil unmount /nix
# sudo diskutil unmount /private/var/run/agenix.d || true

sudo rm -rf \
  /etc/nix \
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

slog "Removing nixbuild group..."
sudo dscl . -delete /Groups/nixbld

slog "Removing old nixbuild users..."
for u in $(sudo dscl . -list /Users | grep _nixbld); do
  sudo dscl . -delete /Users/$u
done

slog "Removing nix entry in /etc/synthetic.conf..."
sudo sed -i '/nix/d' /etc/synthetic.conf

slog "Re-enabling nix-daemon and linux-builder to prep for nix-install.sh..."
# Earlier we disabled the nix-daemon and linux-builder services.  This was to
# help us remove stuff.  We must re-enable them now since the installer isn't
# smart enough to do that.  See
# https://github.com/NixOS/nix/issues/6499#issuecomment-1364200393 for
# additional context.
sudo launchctl enable system/org.nixos.nix-daemon
sudo launchctl kickstart -k system/org.nixos.nix-daemon
sudo launchctl enable system/org.nixos.linux-builder
sudo launchctl kickstart -k system/org.nixos.linux-builder

slog "At this point we should be free to run the install logic - delegating to
nix-install.sh"

./nix-install.sh
