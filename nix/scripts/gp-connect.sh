#!/usr/bin/env bash

################################################################################
# Connect to GlobalProtect VPN using gpclient with SSO authentication.
################################################################################

set -e

# Configuration (can be overridden by environment variables)
GP_SERVER="${GP_SERVER:-}"
GP_GATEWAY="${GP_GATEWAY:-}"
GP_USER="${GP_USER:-$USER}"
GP_BROWSER="${GP_BROWSER:-default}"
GP_RECONNECT_TIMEOUT="${GP_RECONNECT_TIMEOUT:-300}"
GP_LOG_DIR="${GP_LOG_DIR:-$HOME/.local/share/gpclient/logs}"

# Parse arguments
CLEAN=false
VERBOSE=""

while [[ $# -gt 0 ]]; do
  case $1 in
    --server)
      GP_SERVER="$2"
      shift 2
      ;;
    --gateway)
      GP_GATEWAY="$2"
      shift 2
      ;;
    --user)
      GP_USER="$2"
      shift 2
      ;;
    --browser)
      GP_BROWSER="$2"
      shift 2
      ;;
    --clean)
      CLEAN=true
      shift
      ;;
    -v|--verbose)
      VERBOSE="-v"
      shift
      ;;
    -vv|--very-verbose)
      VERBOSE="-vv"
      shift
      ;;
    -h|--help)
      echo "Usage: gp-connect.sh [OPTIONS]"
      echo ""
      echo "Connect to GlobalProtect VPN using SSO authentication."
      echo ""
      echo "Options:"
      echo "  --server TEXT         Portal server URL (required)"
      echo "  --gateway TEXT        Gateway to connect to (optional)"
      echo "  --user TEXT           Username (default: current user)"
      echo "  --browser TEXT        Browser to use: default, firefox, chrome, chromium, remote"
      echo "                        Use 'remote' for headless/manual authentication"
      echo "  --clean               Clear cached authentication cookies"
      echo "  -v, --verbose         Enable verbose output"
      echo "  -vv, --very-verbose   Enable trace output"
      echo "  -h, --help            Show this help message"
      echo ""
      echo "Environment Variables:"
      echo "  GP_SERVER             Portal server URL"
      echo "  GP_GATEWAY            Gateway to connect to"
      echo "  GP_USER               Username (default: \$USER)"
      echo "  GP_BROWSER            Browser to use (default: default)"
      echo "  GP_RECONNECT_TIMEOUT  Reconnection timeout in seconds (default: 300)"
      echo "  GP_LOG_DIR            Log directory (default: ~/.local/share/gpclient/logs)"
      exit 0
      ;;
    *)
      echo "Error: Unknown option $1"
      echo "Use -h or --help for usage information"
      exit 1
      ;;
  esac
done

if [ -z "$GP_SERVER" ]; then
  echo "Error: Portal server is required"
  echo "Use -h or --help for usage information"
  exit 1
fi

# Create log directory if it doesn't exist
mkdir -p "$GP_LOG_DIR"

# Build the gpclient command
GP_CMD="gpclient connect"
GP_CMD="$GP_CMD $VERBOSE"
GP_CMD="$GP_CMD --user \"$GP_USER\""
GP_CMD="$GP_CMD --browser \"$GP_BROWSER\""
GP_CMD="$GP_CMD --reconnect-timeout $GP_RECONNECT_TIMEOUT"

if [ "$CLEAN" = true ]; then
  GP_CMD="$GP_CMD --clean"
fi

if [ -n "$GP_GATEWAY" ]; then
  GP_CMD="$GP_CMD --gateway \"$GP_GATEWAY\""
fi

GP_CMD="$GP_CMD \"$GP_SERVER\""

echo "Connecting to GlobalProtect VPN..."
echo "Server: $GP_SERVER"
if [ -n "$GP_GATEWAY" ]; then
  echo "Gateway: $GP_GATEWAY"
fi
echo "User: $GP_USER"
echo "Browser: $GP_BROWSER"

if [ "$GP_BROWSER" = "remote" ]; then
  echo ""
  echo "Remote browser mode: gpclient will provide an authentication URL."
  echo "Use this URL for headless automation or manual authentication."
  echo ""
fi

echo "Running: $GP_CMD"
echo ""

# Execute the command
eval "$GP_CMD"
