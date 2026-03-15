#!/usr/bin/env bash
################################################################################
# Webhook receiver script for handling Gitea push events.
#
# This script is executed by socat for each incoming HTTP request.  It reads
# HTTP headers and body, checks for push events to master, and triggers the
# deployment check service.
################################################################################

set -euo pipefail

# Read HTTP headers.
while IFS= read -r line; do
  line=$(echo "$line" | tr -d '\r')
  if [[ -z "$line" ]]; then
    break
  fi
  if [[ "$line" =~ ^X-Gitea-Signature: ]]; then
    SIGNATURE="${line#X-Gitea-Signature: }"
  fi
  if [[ "$line" =~ ^Content-Length: ]]; then
    CONTENT_LENGTH="${line#Content-Length: }"
  fi
done

# Read body.
if [[ -n "${CONTENT_LENGTH:-}" ]]; then
  BODY=$(head -c "$CONTENT_LENGTH")
fi

# Verify signature (simplified - in production use proper HMAC verification).
# For now, just check if we got a push event.
if echo "${BODY:-}" | jq -e '.ref == "refs/heads/master"' >/dev/null 2>&1; then
  echo "Push to master detected, triggering deployment check..."
  systemctl start nix-deployment-check.service
fi

# Send HTTP response.
echo -e "HTTP/1.1 200 OK\r\n\r\n"
