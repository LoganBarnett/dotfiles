#!/usr/bin/env bash

################################################################################
# Updates static.nix with the new version and hashes for `makemkv`.
#
# MakeMKV consists of two components:
# - makemkv-oss: Open source component
# - makemkv-bin: Proprietary binary component
#
# Both must be updated to the same version.
################################################################################

set -euo pipefail

# Fetch latest version from MakeMKV forum.
version="$(curl --location --silent \
  'https://forum.makemkv.com/forum/viewtopic.php?f=3&t=224' \
  | grep -oP 'MakeMKV \K[0-9]+\.[0-9]+\.[0-9]+' \
  | head -n1
)"

if [[ -z "$version" ]]; then
  echo "ERROR: Could not determine latest MakeMKV version" >&2
  exit 1
fi

echo "Latest version: ${version}"

# Fetch OSS component hash.
echo "Fetching makemkv-oss hash..."
temp_oss="$(mktemp)"
trap 'rm -f "${temp_oss}" "${temp_bin}"' EXIT
curl --location --silent --fail \
  --output "${temp_oss}" \
  "https://www.makemkv.com/download/makemkv-oss-${version}.tar.gz"
hash_oss_sri="$(nix hash file --type sha256 --sri "${temp_oss}")"

# Fetch BIN component hash.
echo "Fetching makemkv-bin hash..."
temp_bin="$(mktemp)"
curl --location --silent --fail \
  --output "${temp_bin}" \
  "https://www.makemkv.com/download/makemkv-bin-${version}.tar.gz"
hash_bin_sri="$(nix hash file --type sha256 --sri "${temp_bin}")"

echo ""
echo "Version: ${version}"
echo "OSS Hash: ${hash_oss_sri}"
echo "BIN Hash: ${hash_bin_sri}"
echo ""

# Update static.nix using nix-editor.
cd "$(dirname "$0")/.."

nix-editor \
  static.nix \
  'makemkv.version' \
  --val "\"$version\"" \
  --inplace

nix-editor \
  static.nix \
  'makemkv.oss.hash' \
  --val "\"$hash_oss_sri\"" \
  --inplace

nix-editor \
  static.nix \
  'makemkv.bin.hash' \
  --val "\"$hash_bin_sri\"" \
  --inplace \
  --format

echo "Updated static.nix successfully"
