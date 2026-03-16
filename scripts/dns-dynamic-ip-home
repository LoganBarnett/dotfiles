#!/usr/bin/env bash
# shellcheck disable=SC2154
# Disable undeclared variables.  It doesn't work with dynamic variables like
# we have in missing_var.  We have -u for that.  Unfortunately this must be
# declared file-wide or then we have to splay it out everywhere, which defeats
# the purpose of having the dynamic variables saving us from writing a bunch of
# boilerplate in the first place.
#
# Unfortunately, setting this here doesn't work in the Nix context in which it's
# used.  You'll have to use `excludeShellChecks = [ "-e SC2154"; ];` in the
# `writeShellApplication` arguments to disable this.  I believe this is because
# `writeShellApplication` prefixes the script with some commands which then
# trips shellcheck's check of whether or not the disable declaration is the
# "first" thing that happens.

################################################################################
# Synchronizes the detected WAN IP with the DNS server.  If the IPs are the
# same, no operation is performed (it does not force).
#
# This is really just a way to adapt our secrets from agenix to our program.
# This could be generalized to call any program potentially.
################################################################################

set -euo pipefail

while true; do
  case "${1:-}" in
    -h | --help)
      cat <<EOH
Usege: $0 [opts]

  --config-file   <path>   Path to the dness config file.
  --api-key-file  <key>    The API key for the DNS server (currently only
                           Porkbun).
  --secret-file   <secret> The secret for the DNS server (currently only
                           Porkbun).

Synchronizes the detected WAN IP with the DNS server.  If the IPs are the same,
no operation is performed (it does not force).

EOH
      exit
      ;;
   --config-file)
     config_file="${2:-}"
     shift 2
     ;;
   --api-key-file)
     api_key_file="${2:-}"
     shift 2
     ;;
   --secret-file)
     secret_file="${2:-}"
     shift 2
     ;;
   * ) break ;;
  esac
done

missing_var() {
  local name="$1"
  if declare -p "$name" &>/dev/null; then
    declare -g "$1"_missing=false
  else
    declare -g "$1"_missing=true
  fi
}
missing_var 'config_file'
missing_var 'api_key_file'
missing_var 'secret_file'

if [[ \
    $config_file_missing == true \
    || $api_key_file_missing == true \
    || $secret_file_missing == true \
]]; then
  echo "The follow required variables are missing:
--config-file:   $config_file_missing
--api-key-file:  $api_key_file_missing
--secret-file:   $secret_file_missing
" >&2
  exit 1
fi

API_KEY="$(cat "$api_key_file")"
API_SECRET="$(cat "$secret_file")"
export API_KEY
export API_SECRET
dness --config "$config_file"
