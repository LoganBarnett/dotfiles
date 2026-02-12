#!/usr/bin/env bash

################################################################################
# Updates static.nix with the new version and hashes for `claude-code`.
################################################################################

set -euo pipefail

# Get latest version from npm.
version="$(curl --location --silent \
  https://registry.npmjs.org/@anthropic-ai/claude-code \
    | jq --raw-output '.["dist-tags"].latest'
)"

# Fetch and compute hash for the npm tarball.
hash_32="$(nix-prefetch-url \
  --type sha256 \
  --unpack \
  "https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-${version}.tgz"
)"
hash_sri="$(nix hash convert --hash-algo sha256 --to sri "${hash_32}")"

echo "Version: ${version}"
echo "Hash: ${hash_sri}"

# Update version and hash in static.nix.
nix-editor \
  static.nix \
  'claude-code.version' \
  --val "\"$version\"" \
  --inplace

nix-editor \
  static.nix \
  'claude-code.hash' \
  --val "\"$hash_sri\"" \
  --inplace

# Compute npmDepsHash by attempting to build with wrong hash and capturing error.
echo "Computing npmDepsHash..."

# Temporarily set npmDepsHash to empty to trigger hash mismatch error.
nix-editor \
  static.nix \
  'claude-code.npmDepsHash' \
  --val '""' \
  --inplace

# Try to build and capture the expected hash from the error message.
npmDepsHash="$(nix build .#claude-code 2>&1 \
  | sed -nE 's/.*got:[[:space:]]*sha256-([A-Za-z0-9+/=-]+).*/sha256-\1/p' \
  | head -1 \
  || true)"

if [ -z "$npmDepsHash" ]; then
  echo "Warning: Could not compute npmDepsHash automatically."
  echo "Please run: nix build .#claude-code"
  echo "And update static.nix with the hash from the error message."
  # Restore the old hash.
  nix-editor \
    static.nix \
    'claude-code.npmDepsHash' \
    --val '"sha256-VWw1bYkFch95JDlOwKoTAQYOr8R80ICJ8QUI4E64W7o="' \
    --inplace
else
  echo "npmDepsHash: ${npmDepsHash}"
  # Update with the correct hash.
  nix-editor \
    static.nix \
    'claude-code.npmDepsHash' \
    --val "\"$npmDepsHash\"" \
    --inplace \
    --format
fi
