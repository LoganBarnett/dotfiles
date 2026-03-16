################################################################################
# Manage Private Wi-Fi settings on macOS.
#
# macOS has a notion of private Wi-Fi settings.  It has 3 modes:
# Off - It does nothing.
# Fixed: It uses a randomly generated MAC address, but fixed for the SSID.
# Random: It uses a random MAC address anytime it connects to the SSID.
#
# In both Fixed and Random modes, it will offer the machine type ("MacBookPro")
# as the host name when querying DHCP, which can muck with any sort of name
# based profiling you might do.
################################################################################
{ ... }: {
  # TODO: Test that this actually works, and probably stuff it into a script
  # that's easier to reference.
  # TODO: This is currently auto-detecting the SSID, but this should be static.
  # We could make an "auto" section for all modes going forward, but that
  # probably extends beyond what we need here.
  system.activationScripts.postActivation.text = ''
    set -euo pipefail

    ssid="$(
      /System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport -I \
        | awk -F': ' '$1 ~ /^ *SSID$/ { print $2; exit }'
                                                                   )"

    if [[ -z "''${ssid}" ]]; then
      echo "No SSID detected; skipping Private Wi-Fi Address override." >&2
      exit 0
    fi

    plist="/Library/Preferences/com.apple.wifi.known-networks.plist"
    key="wifi.network.ssid.''${ssid}.PrivateMACAddressModeUserSetting"

    # Set per-SSID private MAC mode to "off" (unsupported Apple internal setting).
    # Note: may require reboot to take effect; UI may not reflect the change immediately.
    # See: bruner blog post.
    # (Run as root)
    # shellcheck disable=SC2016
    /usr/bin/sudo /usr/bin/plutil --replace "''${key}" --string off "''${plist}" \
      || echo "Could not set key (SSID may not be present in known-networks yet): ''${key}" >&2
  '';
}
