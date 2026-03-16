#!/usr/bin/env bash

################################################################################
# Monitor GlobalProtect VPN connection and reconnect if disconnected.
# This script is designed to run continuously as a background service.
################################################################################

set -e

# Configuration (can be overridden by environment variables)
GP_SERVER="${GP_SERVER:-}"
GP_GATEWAY="${GP_GATEWAY:-}"
GP_USERNAME="${GP_USERNAME:-}"
ORG_NAME="${ORG_NAME:-}"
GP_CHECK_INTERVAL="${GP_CHECK_INTERVAL:-60}"
GP_LOG_DIR="${GP_LOG_DIR:-$HOME/.local/share/gpclient/logs}"
GP_MONITOR_LOG="${GP_MONITOR_LOG:-$GP_LOG_DIR/monitor.log}"

# Parse arguments
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
    --username)
      GP_USERNAME="$2"
      shift 2
      ;;
    --org-name)
      ORG_NAME="$2"
      shift 2
      ;;
    --check-interval)
      GP_CHECK_INTERVAL="$2"
      shift 2
      ;;
    -h|--help)
      echo "Usage: gp-monitor.sh [OPTIONS]"
      echo ""
      echo "Monitor GlobalProtect VPN connection and reconnect if disconnected."
      echo "Uses gp-connect-auto for fully automated SSO + TOTP authentication."
      echo ""
      echo "Options:"
      echo "  --server TEXT           Portal server URL (required)"
      echo "  --gateway TEXT          Gateway to connect to (optional)"
      echo "  --username TEXT         VPN username/email (required)"
      echo "  --org-name TEXT         Organization name for pass entries (required)"
      echo "  --check-interval N      Seconds between connection checks (default: 60)"
      echo "  -h, --help              Show this help message"
      echo ""
      echo "Environment Variables:"
      echo "  GP_SERVER               Portal server URL"
      echo "  GP_GATEWAY              Gateway to connect to"
      echo "  GP_USERNAME             VPN username/email"
      echo "  ORG_NAME                Organization name for pass entries"
      echo "  GP_CHECK_INTERVAL       Seconds between checks (default: 60)"
      echo "  GP_LOG_DIR              Log directory (default: ~/.local/share/gpclient/logs)"
      echo "  GP_MONITOR_LOG          Monitor log file (default: \$GP_LOG_DIR/monitor.log)"
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
  echo "Error: Portal server is required (GP_SERVER)"
  echo "Use -h or --help for usage information"
  exit 1
fi

if [ -z "$GP_USERNAME" ]; then
  echo "Error: Username is required (GP_USERNAME)"
  echo "Use -h or --help for usage information"
  exit 1
fi

if [ -z "$ORG_NAME" ]; then
  echo "Error: Organization name is required (ORG_NAME)"
  echo "Use -h or --help for usage information"
  exit 1
fi

# Create log directory if it doesn't exist
mkdir -p "$GP_LOG_DIR"

# Logging function
log() {
  echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" | tee -a "$GP_MONITOR_LOG"
}

# Check if VPN is connected
is_connected() {
  # Check if gpclient is running and has established a connection
  # Look for the PID file and verify the process is running
  if [ -f /var/run/gpclient.lock ]; then
    PID=$(cat /var/run/gpclient.lock 2>/dev/null)
    if [ -n "$PID" ] && kill -0 "$PID" 2>/dev/null; then
      return 0
    fi
  fi
  return 1
}

# Disconnect function
disconnect() {
  log "Disconnecting from GlobalProtect VPN..."
  gpclient disconnect || true
  sleep 2
}

# Connect function
connect() {
  log "Connecting to GlobalProtect VPN using automated authentication..."
  log "Server: $GP_SERVER"
  if [ -n "$GP_GATEWAY" ]; then
    log "Gateway: $GP_GATEWAY"
  fi
  log "User: $GP_USERNAME"

  # Use gp-connect-auto for fully automated connection
  # This handles SSO, TOTP, and tunnel creation automatically
  gp-connect-auto >> "$GP_MONITOR_LOG" 2>&1 &
  CONNECT_PID=$!

  log "Automated connection process started (PID: $CONNECT_PID)"

  # Wait for connection to establish (with timeout)
  # Increased timeout for automated authentication flow
  WAIT_COUNT=0
  MAX_WAIT=120
  while [ $WAIT_COUNT -lt $MAX_WAIT ]; do
    if is_connected; then
      log "VPN connection established successfully"

      # Fix DNS scoping on macOS
      log "Fixing DNS scoping for VPN interface..."
      if fix-vpn-dns-scoping >> "$GP_MONITOR_LOG" 2>&1; then
        log "DNS scoping fixed successfully"
      else
        log "WARNING: Failed to fix DNS scoping"
      fi

      return 0
    fi
    sleep 1
    WAIT_COUNT=$((WAIT_COUNT + 1))
  done

  log "WARNING: VPN connection did not establish within $MAX_WAIT seconds"
  return 1
}

# Main monitoring loop
log "Starting GlobalProtect VPN monitor"
log "Check interval: ${GP_CHECK_INTERVAL}s"

# Initial connection attempt
if ! is_connected; then
  log "VPN is not connected, initiating connection..."
  connect
else
  log "VPN is already connected"
fi

# Monitor loop
while true; do
  sleep "$GP_CHECK_INTERVAL"

  if ! is_connected; then
    log "VPN connection lost, attempting to reconnect..."
    disconnect
    sleep 2
    connect
  else
    log "VPN connection is active"
  fi
done
