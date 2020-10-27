#!/usr/bin/env bash
set -euo pipefail

sudo ls # To ensure we're in a sudo context first.
# Pipe yes because there's no way to do an officially unattended install. Though
# piping anything to it removes the tty and then it works unattended...
#
# Even with all of this, the installer will crash and burn and the slightest
# presence of itself. Upgrading is a different command which we should run
# immediately.
yes | sh <(curl -L https://nixos.org/nix/install) --daemon || true

# Their generated script checks first, but it better damn sure be there or this
# endeavor is in error.
. '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'

# Attempt to upgrade.
sudo -i sh -c 'nix-channel --update && \
  nix-env -iA nixpkgs.nix && \
  launchctl remove org.nixos.nix-daemon && \
  launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist'

mkdir -p ~/.config/nixpkgs
# Link the file for this machine. This allows for host specific configurations.
machineName=$(scutil --get ComputerName | cut -d' ' -f1 | tr -d $'\n' || hostname)
ln -snf $PWD/$machineName.nix ~/.config/nixpkgs/config.nix

nix-env -iA nixpkgs.shellPackages

# The guide recommends the below flag for catalina.
# https://nixos.org/manual/nix/stable/#chap-quick-start
# --darwin-use-unencrypted-nix-store-volume

echo "Trusting system store..."
./nix-trust-system-store.sh
