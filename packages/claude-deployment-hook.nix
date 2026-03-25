################################################################################
# Claude deployment hook package.
#
# This creates a hook script that Claude can use to check deployment status
# after pushing changes to the repository.
################################################################################
{
  curl,
  domain,
  jq,
  lib,
  writeScriptBin,
}:

writeScriptBin "claude-deployment-hook" ''
  #!/usr/bin/env bash
  # Claude deployment status hook.
  #
  # This script is designed to be called by Claude Code after pushing changes
  # to check deployment status across all hosts.

  set -euo pipefail

  DEPLOYMENT_SERVER="rubidium.${domain}"
  STATUS_URL="http://deployment.$DEPLOYMENT_SERVER/status"

  # Colors for output.
  GREEN='\033[0;32m'
  YELLOW='\033[0;33m'
  RED='\033[0;31m'
  NC='\033[0m' # No Color

  echo "🚀 Checking deployment status..."

  # Function to check a single host's status.
  check_host_status() {
    local host="$1"
    local status_file="$host.status"

    # Fetch status from deployment server.
    if status_data=$(${curl}/bin/curl -s "$STATUS_URL/$status_file" 2>/dev/null); then
      local status=$(echo "$status_data" | head -1)
      local timestamp=$(echo "$status_data" | tail -1)

      case "$status" in
        "SUCCESS")
          echo -e "  ''${GREEN}✓''${NC} $host - Deployed successfully at $timestamp"
          ;;
        "DEPLOYING")
          echo -e "  ''${YELLOW}⟳''${NC} $host - Currently deploying (started $timestamp)"
          return 1
          ;;
        "FAILED")
          echo -e "  ''${RED}✗''${NC} $host - Deployment failed at $timestamp"
          return 1
          ;;
        *)
          echo -e "  ''${YELLOW}?''${NC} $host - Unknown status: $status"
          ;;
      esac
    else
      echo -e "  ''${YELLOW}-''${NC} $host - No deployment status available"
    fi

    return 0
  }

  # Get list of all status files.
  if status_list=$(${curl}/bin/curl -s "$STATUS_URL/" | grep -o 'href="[^"]*\.status"' | sed 's/href="//;s/"//'); then
    all_success=true

    for status_file in $status_list; do
      host=''${status_file%.status}
      if ! check_host_status "$host"; then
        all_success=false
      fi
    done

    echo ""

    if [[ "$all_success" == true ]]; then
      echo -e "''${GREEN}All deployments completed successfully!''${NC}"
    else
      echo -e "''${YELLOW}Some deployments are pending or failed.''${NC}"
      echo "Check logs with: ssh $DEPLOYMENT_SERVER journalctl -u nix-deployment-check -f"
    fi
  else
    echo -e "''${RED}Could not connect to deployment server.''${NC}"
    echo "Ensure $DEPLOYMENT_SERVER is accessible."
    exit 1
  fi

  # Show recent deployment activity.
  echo ""
  echo "Recent deployment activity:"
  if recent_log=$(${curl}/bin/curl -s "$STATUS_URL/../deployment.log" | tail -10 2>/dev/null); then
    echo "$recent_log"
  else
    echo "(Log not available)"
  fi
''
