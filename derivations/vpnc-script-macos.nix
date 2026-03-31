# vpnc-script-macos cannot use writeShellApplication because it relies on
# unset environment variables from gpclient (CISCO_SPLIT_DNS, etc.) and pipe
# patterns that break under -o nounset / -o pipefail.  writeScriptBin
# preserves the script's own shebang and set flags without adding stricter
# ones.
#
# Runtime dependency dns-resolver-helper is provided via systemPackages in
# global-protect-persistent.nix, not bundled here, because this script runs
# in the gpclient root context with the system PATH.
{ writeScriptBin }:
writeScriptBin "vpnc-script-macos" (
  builtins.readFile ../scripts/vpnc-script-macos
)
