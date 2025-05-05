#!/usr/bin/env bash
################################################################################
# Restart a network interface, unattended via SSH.
#
# This is helpful for headless machines where I might want to restart their
# network interface for some reason.  I expect the SSH session will die and
# likely won't come back up (typically these invocations are because I need to
# make big network settings changes).  But I need the process to continue
# regardless.
################################################################################

set -euo pipefail

# Find the first active non-virtual Ethernet interface.
ethernetDevice() {
  for path in /sys/class/net/*; do
    iface=$(basename "$path")
    echo "Trying $iface..." 1>&2
    if [[ -e "/sys/class/net/$iface/device" ]] && ethtool "$iface" | grep -q "Link detected: yes"; then
      echo "$iface"
      return 0
    fi
  done
  echo "Error: Could not find a suitable device." 1>&2
  return 1
}

iface=$(ethernetDevice)

if [[ -z "$iface" ]]; then
  echo "No suitable interface found."
  exit 1
fi

echo "Restarting interface: '$iface'"

sudo bash -c "
  nohup bash -c '
    ip link set $iface down
    sleep 2
    ip addr flush dev $iface
    ip link set $iface up
    dhcpcd $iface
  ' > /tmp/netlog 2>&1 & disown
"
