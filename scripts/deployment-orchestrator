#!/usr/bin/env bash
# Orchestrate Nix configuration deployment across all managed hosts.  Pulls
# the latest changes from the repository and deploys any host whose
# configuration has changed.  Hosts to check are passed as positional
# arguments; NIX_DEPLOYMENT_REPO_PATH and NIX_DEPLOYMENT_STATUS_DIR must
# be set in the environment.
set -euo pipefail

REPO_PATH="${NIX_DEPLOYMENT_REPO_PATH}"
STATUS_DIR="${NIX_DEPLOYMENT_STATUS_DIR}"

echo "Starting deployment check at $(date -Iseconds)"

# Update the repository.
cd "$REPO_PATH"
git fetch origin
git reset --hard origin/"$(git rev-parse --abbrev-ref HEAD)"

# Check each host for changes and deploy those that have them.
for host in "$@"; do
  echo "Checking $host..."

  if result=$(check-host-changed "$host" "$REPO_PATH"); then
    if echo "$result" | grep -q "^CHANGED"; then
      echo "Host $host has changes, deploying..."
      deploy-host "$host" "$REPO_PATH" "$STATUS_DIR" &
    else
      echo "Host $host unchanged, skipping."
    fi
  else
    echo "Failed to check $host"
  fi
done

# Wait for all background deployments to complete.
wait

echo "Deployment check completed at $(date -Iseconds)"
