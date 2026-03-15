#!/usr/bin/env bash
################################################################################
# Script to configure a Gitea webhook via direct database access.
#
# This script uses PostgreSQL to declaratively manage webhooks, avoiding the
# need for API tokens that become invalid after rebuilds.
#
# Parameters:
#   $1 - Repository owner
#   $2 - Repository name
#   $3 - Webhook URL
#   $4 - Path to webhook secret file
#   $5 - Comma-separated list of events (e.g., "push,create")
################################################################################

set -euo pipefail

# Parse arguments.
REPO_OWNER="${1:?Repository owner required}"
REPO_NAME="${2:?Repository name required}"
WEBHOOK_URL="${3:?Webhook URL required}"
WEBHOOK_SECRET_FILE="${4:?Webhook secret file required}"
EVENTS="${5:-push}"

echo "Configuring webhook for $REPO_OWNER/$REPO_NAME..."

# Read the webhook secret.
WEBHOOK_SECRET=$(cat "$WEBHOOK_SECRET_FILE")

# Check if repository exists by querying the database directly.
REPO_EXISTS=$(psql -U gitea -d gitea -tAc \
  "SELECT EXISTS(SELECT 1 FROM repository WHERE owner_name='$REPO_OWNER' AND name='$REPO_NAME');" \
  || echo "f")

if [ "$REPO_EXISTS" != "t" ]; then
  echo "Repository $REPO_OWNER/$REPO_NAME not found, skipping..."
  exit 0
fi

# Use direct database access to manage webhooks.
# Get repository ID first.
REPO_ID=$(psql -U gitea -d gitea -v owner="$REPO_OWNER" -v name="$REPO_NAME" -tA <<'SQL'
SELECT id FROM repository WHERE owner_name = :'owner' AND name = :'name';
SQL
)

# Upsert the webhook using a simpler two-step approach.
# First, try to update.
UPDATED=$(psql -U gitea -d gitea -v repo_id="$REPO_ID" -v url="$WEBHOOK_URL" \
  -v secret="$WEBHOOK_SECRET" -v events="$EVENTS" -tA <<'SQL'
UPDATE webhook
SET secret = :'secret', events = :'events', is_active = true,
    updated_unix = EXTRACT(EPOCH FROM NOW())::BIGINT
WHERE repo_id = :'repo_id' AND url = :'url'
RETURNING id;
SQL
)

# If no rows were updated (empty output or "UPDATE 0"), insert.
if [ -z "$UPDATED" ] || [[ "$UPDATED" == "UPDATE 0" ]]; then
  psql -U gitea -d gitea -v repo_id="$REPO_ID" -v url="$WEBHOOK_URL" \
    -v secret="$WEBHOOK_SECRET" -v events="$EVENTS" >/dev/null <<'SQL'
INSERT INTO webhook (
  repo_id, url, content_type, secret, events,
  is_active, type, created_unix, updated_unix
) VALUES (
  :'repo_id', :'url', 1, :'secret', :'events',
  true, 'gitea',
  EXTRACT(EPOCH FROM NOW())::BIGINT,
  EXTRACT(EPOCH FROM NOW())::BIGINT
);
SQL
  echo "Created new webhook for $REPO_OWNER/$REPO_NAME"
else
  echo "Updated existing webhook for $REPO_OWNER/$REPO_NAME"
fi

echo "Webhook configured successfully for $REPO_OWNER/$REPO_NAME"
