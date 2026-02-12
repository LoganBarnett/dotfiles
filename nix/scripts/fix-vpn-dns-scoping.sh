#!/usr/bin/env bash
################################################################################
# Fix GlobalProtect VPN DNS scoping for macOS
# This script updates the DNS configuration to properly scope VPN domains
# to the VPN interface instead of the primary network interface.
################################################################################

set -e

# Find the VPN tunnel interface (utun with 10.89.x.x IP)
VPN_IF=$(ifconfig | grep -B 3 "inet 10.89" | grep "^utun" | head -1 | cut -d: -f1)

if [ -z "$VPN_IF" ]; then
  echo "Error: No VPN tunnel interface found"
  exit 1
fi

echo "Found VPN interface: $VPN_IF"

# Extract DNS servers and search domains from current DNS configuration.
# GlobalProtect sets these but without proper macOS scoping.
echo "Extracting current DNS configuration..."

# Get all DNS configuration and extract servers and domains.
DNS_CONFIG=$(scutil --dns)

# Extract DNS servers (looking for nameserver entries).
VPN_DNS_SERVERS=($(echo "$DNS_CONFIG" | grep "nameserver\[" | awk '{print $3}' | sort -u))

# Extract search domains (looking for domain entries).
VPN_SEARCH_DOMAINS=($(echo "$DNS_CONFIG" | grep -E "domain\[|search domain\[" | awk '{print $4}' | sort -u))

if [ ${#VPN_DNS_SERVERS[@]} -eq 0 ]; then
  echo "Error: No DNS servers found in current configuration"
  exit 1
fi

if [ ${#VPN_SEARCH_DOMAINS[@]} -eq 0 ]; then
  echo "Warning: No search domains found in current configuration"
fi

echo "Found DNS servers: ${VPN_DNS_SERVERS[*]}"
echo "Found search domains: ${VPN_SEARCH_DOMAINS[*]}"

echo "Updating DNS configuration for $VPN_IF with proper scoping..."

# Set DNS configuration using scutil
sudo /usr/sbin/scutil << EOF
d.init
d.add ServerAddresses * ${VPN_DNS_SERVERS[@]}
d.add SearchDomains * ${VPN_SEARCH_DOMAINS[@]}
d.add SupplementalMatchDomains * ${VPN_SEARCH_DOMAINS[@]}
set State:/Network/Service/$VPN_IF/DNS
quit
EOF

# Notify the system to reconfigure DNS
sudo /usr/sbin/scutil << EOF
notify State:/Network/Global/DNS
quit
EOF

echo "DNS configuration updated for $VPN_IF"
echo "DNS servers: ${VPN_DNS_SERVERS[*]}"
echo "Search domains: ${VPN_SEARCH_DOMAINS[*]}"
