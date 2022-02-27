#!/usr/bin/env bash
set -euo pipefail

# Any macOS specific installation requirements should go here.

dir="$(dirname "${BASH_SOURCE[0]}")"
source bash-logging

slog "Installing XCode..."
xcode-select --install || true

# Without this, things like curl wouldn't work for our trusted certificates
# (primarily for private networks).
slog "Adding system trusts to nix tools..."
sudo sh -c \
  'security find-certificate -a \
  -p /Library/Keychains/System.keychain > \
  /nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt'
sudo sh -c \
  'security find-certificate -a \
  -p /System/Library/Keychains/SystemRootCertificates.keychain >> \
  /nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt'

slog "Configuring keyboard"
./darwin-keyboard-install.sh
