#!/usr/bin/env bash

################################################################################
# Example script showing how to extract the authentication URL from gpclient
# when running in remote browser mode.
#
# This is a starting point for building headless automation.
################################################################################

set -e

GP_SERVER="${GP_SERVER:-}"

if [ -z "$GP_SERVER" ]; then
  echo "Error: GP_SERVER environment variable is not set"
  exit 1
fi

echo "Starting gpclient in remote browser mode..."
echo "This will give us an authentication URL for automation."
echo ""

# Start gpclient in background and capture output
{
  gpclient connect --browser remote -vv "$GP_SERVER" 2>&1 | while IFS= read -r line; do
    # Echo all output to see what's happening
    echo "$line"

    # Extract the authentication URL when it appears
    if [[ "$line" =~ http://[0-9.]+:[0-9]+/[a-f0-9-]+ ]]; then
      AUTH_URL="${BASH_REMATCH[0]}"
      echo ""
      echo "============================================================"
      echo "AUTHENTICATION URL EXTRACTED:"
      echo "$AUTH_URL"
      echo "============================================================"
      echo ""
      echo "At this point, you would:"
      echo "1. Pass this URL to your headless browser automation"
      echo "2. Complete the SSO authentication programmatically"
      echo "3. Handle the TOTP/MFA challenge"
      echo "4. Let the callback complete to the local server"
      echo ""
      echo "For now, open this URL in your browser manually to complete auth."
      echo "gpclient is waiting for the callback..."
      echo ""
    fi
  done
} &

GPCLIENT_PID=$!

# Wait for gpclient to complete
wait $GPCLIENT_PID

echo ""
echo "VPN connection established!"
