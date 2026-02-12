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

# VPN DNS configuration
VPN_DNS_SERVERS=("10.198.51.37" "10.198.51.38" "10.36.5.135")
VPN_SEARCH_DOMAINS=(
  "americas.nwea.pvt"
  "nwea.pvt"
  "nweacolo.pvt"
  "hegn.us"
  "pubedu.hegn.us"
  "hmco.com"
  "rpcsys.hmco.com"
  "hmhco.com"
  "hebnh.local"
  "heinemann.com"
  "hmhpub.com"
  "sap2.hmhco.com"
)

echo "Updating DNS configuration for $VPN_IF..."

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
