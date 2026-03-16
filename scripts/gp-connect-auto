#!/usr/bin/env bash

################################################################################
# Automatically connect to GlobalProtect VPN using headless browser automation.
# Handles SSO authentication and TOTP/MFA challenges automatically.
################################################################################

set -e

# Configuration
GP_SERVER="${GP_SERVER:-}"
GP_GATEWAY="${GP_GATEWAY:-}"
GP_USERNAME="${GP_USERNAME:-}"
GP_PASS_ENTRY="${GP_PASS_ENTRY:-${ORG_NAME}}"
GP_TOTP_ENTRY="${GP_TOTP_ENTRY:-${ORG_NAME}-totp}"
GP_LOG_DIR="${GP_LOG_DIR:-$HOME/.local/share/gpclient/logs}"

# Parse arguments
VERBOSE=""
CLEAN=false

while [[ $# -gt 0 ]]; do
  case $1 in
    -v|--verbose)
      VERBOSE="-v"
      shift
      ;;
    -vv|--very-verbose)
      VERBOSE="-vv"
      shift
      ;;
    --clean)
      CLEAN=true
      shift
      ;;
    -h|--help)
      echo "Usage: gp-connect-auto [OPTIONS]"
      echo ""
      echo "Automatically connect to GlobalProtect VPN with headless authentication."
      echo ""
      echo "Options:"
      echo "  --clean               Clear cached authentication cookies"
      echo "  -v, --verbose         Enable verbose output"
      echo "  -vv, --very-verbose   Enable trace output"
      echo "  -h, --help            Show this help message"
      echo ""
      echo "Environment Variables:"
      echo "  GP_SERVER             Portal server URL (required)"
      echo "  GP_USERNAME           VPN username (email address) (required)"
      echo "  ORG_NAME              Organization name (required for default pass entries)"
      echo "  GP_PASS_ENTRY         Pass entry for password (default: \$ORG_NAME)"
      echo "  GP_TOTP_ENTRY         Pass entry for TOTP (default: \$ORG_NAME-totp)"
      echo ""
      echo "Credentials are retrieved from 'pass':"
      echo "  Password: pass show \$GP_PASS_ENTRY"
      echo "  TOTP:     pass otp \$GP_TOTP_ENTRY"
      exit 0
      ;;
    *)
      echo "Error: Unknown option $1"
      echo "Use -h or --help for usage information"
      exit 1
      ;;
  esac
done

# Validate environment
if [ -z "$GP_SERVER" ]; then
  echo "Error: GP_SERVER environment variable is not set"
  exit 1
fi

if [ -z "$GP_USERNAME" ]; then
  echo "Error: GP_USERNAME environment variable is not set"
  exit 1
fi

if [ -z "$ORG_NAME" ]; then
  echo "Error: ORG_NAME environment variable is not set"
  exit 1
fi

# Check if pass is available
if ! command -v pass &> /dev/null; then
  echo "Error: 'pass' command not found"
  exit 1
fi

# Create log directory
mkdir -p "$GP_LOG_DIR"

echo "============================================================"
echo "GlobalProtect Automatic Connection"
echo "============================================================"
echo "Server:   $GP_SERVER"
echo "User:     $GP_USERNAME"
echo "Password: Retrieved from pass ($GP_PASS_ENTRY)"
echo "TOTP:     Generated from pass otp ($GP_TOTP_ENTRY)"
echo ""

# Retrieve credentials
echo "Retrieving credentials from pass..."
PASSWORD=$(pass show "$GP_PASS_ENTRY" | head -1)
if [ -z "$PASSWORD" ]; then
  echo "Error: Failed to retrieve password from pass"
  exit 1
fi
echo "✓ Password retrieved"

TOTP_CODE=$(pass otp "$GP_TOTP_ENTRY")
if [ -z "$TOTP_CODE" ]; then
  echo "Error: Failed to retrieve TOTP code from pass"
  exit 1
fi
echo "✓ TOTP code generated: $TOTP_CODE"
echo ""

# Build gpclient command
GP_CMD="gpclient connect --browser remote $VERBOSE"
if [ "$CLEAN" = true ]; then
  GP_CMD="$GP_CMD --clean"
fi
if [ -n "$GP_GATEWAY" ]; then
  GP_CMD="$GP_CMD --gateway \"$GP_GATEWAY\""
fi
# Use custom macOS vpnc-script if available
if [ -n "$GP_VPNC_SCRIPT" ] && [ -f "$GP_VPNC_SCRIPT" ]; then
  GP_CMD="$GP_CMD --script \"$GP_VPNC_SCRIPT\""
fi
GP_CMD="$GP_CMD \"$GP_SERVER\""

echo "Starting gpclient in remote browser mode with PTY..."
echo "Running: $GP_CMD"
echo ""

# Use Python PTY wrapper to run gpclient
# It will handle authentication automatically
# Pass the actual script path, not the function name
gp-connect-pty.py "$GP_CMD" "$GP_USERNAME" "$PASSWORD" "$TOTP_CODE" "$AUTH_SCRIPT"
GPCLIENT_EXIT=$?

if [ $GPCLIENT_EXIT -eq 0 ]; then
  echo ""
  echo "============================================================"
  echo "✓ VPN connection established successfully!"
  echo "============================================================"
  echo ""
  echo "Check connection status:"
  echo "  ifconfig gpd0"
  echo ""
  exit 0
else
  echo ""
  echo "============================================================"
  echo "✗ VPN connection failed"
  echo "============================================================"
  echo ""
  echo "Check logs in: $GP_LOG_DIR"
  echo "Check screenshots in: /tmp/gp-auth-*.png"
  exit 1
fi
