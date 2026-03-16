#!/usr/bin/env bash
################################################################################
# macOS-specific vpnc-script for GlobalProtect/OpenConnect
# This script properly configures DNS using scutil instead of networksetup
################################################################################

set -e

# Environment variables set by gpclient/openconnect:
# TUNDEV - tunnel device name (e.g., utun5)
# INTERNAL_IP4_ADDRESS - VPN IP address
# INTERNAL_IP4_DNS - DNS servers (space-separated)
# CISCO_SPLIT_DNS - DNS search domains (space-separated)

case "$reason" in
  connect)
    echo "VPN connecting on $TUNDEV..."

    # Set up DNS using scutil
    if [ -n "$INTERNAL_IP4_DNS" ]; then
      # Convert space-separated DNS servers to array
      DNS_SERVERS=($INTERNAL_IP4_DNS)
      SEARCH_DOMAINS=($CISCO_SPLIT_DNS)

      # Get the interface index for the tunnel device
      # On macOS, we need to find the if_index programmatically
      IFINDEX=$(ifconfig $TUNDEV 2>/dev/null | head -1 | sed 's/.*flags=.*//' || echo "")

      # Build scutil commands
      # First, set DNS for the VPN service
      scutil << EOF
d.init
d.add ServerAddresses * ${DNS_SERVERS[@]}
$(if [ ${#SEARCH_DOMAINS[@]} -gt 0 ]; then echo "d.add SearchDomains * ${SEARCH_DOMAINS[@]}"; fi)
$(if [ ${#SEARCH_DOMAINS[@]} -gt 0 ]; then echo "d.add DomainName ${SEARCH_DOMAINS[0]}"; fi)
$(if [ ${#SEARCH_DOMAINS[@]} -gt 0 ]; then echo "d.add SupplementalMatchDomains * ${SEARCH_DOMAINS[@]}"; fi)
set State:/Network/Service/$TUNDEV/DNS
quit
EOF

      echo "DNS configured: ${DNS_SERVERS[@]}"
      echo "Search domains: ${SEARCH_DOMAINS[@]}"

      # Notify system to reconfigure DNS
      echo "Notifying system of DNS changes..."
      /usr/sbin/scutil << EOF
notify State:/Network/Global/DNS
quit
EOF
    fi

    # Set up routes (handled by gpclient, but log them)
    if [ -n "$CISCO_SPLIT_INC" ]; then
      echo "Split tunnel routes configured"
    fi
    ;;

  disconnect)
    echo "VPN disconnecting from $TUNDEV..."

    # Remove DNS configuration
    scutil << EOF
remove State:/Network/Service/$TUNDEV/DNS
quit
EOF

    echo "DNS configuration removed"
    ;;

  *)
    echo "Unknown reason: $reason"
    ;;
esac

exit 0
