#!/usr/bin/env bash
set -euo pipefail

HOST="$1"
FLAKE_PATH="$2"

# Get the current system path for the host.
CURRENT_PATH=$(ssh "$HOST" readlink /run/current-system 2>/dev/null || echo "")

# Build the new system path without activating.
NEW_PATH=$(nix build --no-link --print-out-paths \
  "$FLAKE_PATH#nixosConfigurations.$HOST.config.system.build.toplevel" \
  2>/dev/null || echo "")

if [[ -z "$CURRENT_PATH" || -z "$NEW_PATH" ]]; then
  echo "ERROR: Could not determine system paths"
  exit 1
fi

if [[ "$CURRENT_PATH" != "$NEW_PATH" ]]; then
  echo "CHANGED"
  echo "Current: $CURRENT_PATH"
  echo "New: $NEW_PATH"
else
  echo "UNCHANGED"
fi
