#!/usr/bin/env bash

################################################################################
# Updates static.nix with the new version and hash for `signal-desktop-bin`.
################################################################################

set -euo pipefail

# nixcfg --file statics.nix --in-place set signalDesktop.hash "\"sha256-NEWHASH\""
version="$(curl --location --silent \
  https://api.github.com/repos/signalapp/Signal-Desktop/releases/latest \
    | jq --raw-output '.tag_name'
)"
hash_32="$(nix-prefetch-url \
  --type sha256 \
  --name signal-desktop-bin-${version//./-} \
  "https://updates.signal.org/desktop/signal-desktop-mac-universal-${version#v}.dmg"
)"
hash_sri="$(nix hash convert --hash-algo sha256 --to sri "${hash_32}")"
echo "Version: ${version#v}"
echo "Hash: ${hash_sri}"
nix-editor \
  static.nix \
  'signal-desktop-bin.hash' \
  --val "\"$hash_sri\"" \
  --inplace
nix-editor \
  static.nix \
  'signal-desktop-bin.version' \
  --val "\"${version#v}\"" \
  --inplace \
  --format
