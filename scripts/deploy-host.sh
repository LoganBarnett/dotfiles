#!/usr/bin/env bash
set -euo pipefail

HOST="$1"
FLAKE_PATH="$2"
STATUS_DIR="$3"

echo "Deploying to $HOST..."

STATUS_FILE="$STATUS_DIR/$HOST.status"
echo "DEPLOYING" > "$STATUS_FILE"
date -Iseconds >> "$STATUS_FILE"

if nixos-rebuild switch \
     --flake "$FLAKE_PATH#$HOST" \
     --target-host "$HOST" \
     --use-remote-sudo \
     2>&1 | tee -a "$STATUS_FILE.log"; then
  echo "SUCCESS" > "$STATUS_FILE"
  date -Iseconds >> "$STATUS_FILE"
else
  echo "FAILED" > "$STATUS_FILE"
  date -Iseconds >> "$STATUS_FILE"
fi
