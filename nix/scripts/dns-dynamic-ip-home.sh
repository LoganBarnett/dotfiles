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
# Note, this does not yet respect rate limits.  However, according to this
# post, the limit is 60: https://developer.godaddy.com/getstarted
# See also https://github.com/libdns/godaddy/issues/7 as a potential thing to
# use instead of curl, but honestly curl is working just fine here, especially
# since this tool doesn't respect rate limits, and doesn't seem to provide a
# CLI.
################################################################################

set -euo pipefail

domain=""
hostname=""
while true; do
  case "${1:-}" in
    -h | --help)
      cat <<EOH
Usege: $0 [opts]

  --hostname      <name>   The hostname section of the FQDN to sync (e.g.
                           'www').
  --domain        <name>   The domain section of the FQDN to sync (e.g.
                           'foobar.com').
  --api-key-file  <key>    The API key for the DNS server (currently only
                           GoDaddy).
  --secret-file   <secret> The secret for the DNS server (currently only
                           GoDaddy).

Synchronizes the detected WAN IP with the DNS server.  If the IPs are the same,
no operation is performed (it does not force).

Do not run this more than 60 times per minute, per GoDaddy's rate limit stated
here: https://developer.godaddy.com/getstarted

EOH
      exit
      ;;
   --hostname)
     hostname="${2:-}"
     shift 2
     ;;
   --domain)
     domain="${2:-}"
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
  if [[ "${!1}" != '' ]]; then
    declare -g "$1"_missing=false
  else
    declare -g "$1"_missing=true
  fi
}
missing_var 'hostname'
missing_var 'domain'
missing_var 'api_key_file'
missing_var 'secret_file'

if [[ \
  $hostname_missing == true \
    || $domain_missing == true \
    || $api_key_file_missing == true \
    || $secret_file_missing == true \
]]; then
  echo "The follow required variables are missing:
--hostname:      $hostname_missing
--domain:        $domain_missing
--api-key-file:  $api_key_file_missing
--secret-file:   $secret_file_missing
"
  exit 1
fi

api_key='9EE9DPYvR4r_EyaKRBRwZZcoMYW7ULVB6i'
secret='SUAMFxDwaujoCRjVjM4K4n'

#sso_key="$(cat "$api_key_file"):$(cat "$secret_file")"
sso_key="$api_key:$secret"
fqdn="${hostname}.${domain}"

current_time() {
  printf '[%s]' "$(date '+%Y-%m-%d %H:%M:%S')"
}

slog() {
  echo "$(current_time) " "$@" 2>&1
}

curl_godaddy() {
  slog "Alas, GoDaddy shut off API access to those with < 50 domains."
  exit 1
  # We actually want to unsplat the arguments here.
  # shellcheck disable=SC2068
  curl \
    --silent \
    --header "Authorization: sso-key ${sso_key}" \
    $@
}


slog 'Getting WAN IP...'
wan_ip="$(curl --silent 'https://api.ipify.org')"
slog 'Getting DNS IP...'
dns_ip=$(curl_godaddy \
  --request GET \
  "https://api.godaddy.com/v1/domains/${domain}/records/A/${hostname}"
)

slog "Current WAN IP for ${fqdn} is '$wan_ip', DNS IP is '$dns_ip'."

if [[ "$wan_ip" != "" ]]; then
  curl_godaddy \
    --request PUT \
    --header 'Content-Type: application/json' \
    --data "[{\"data\": \"${wan_ip}\"}]"
  slog "Updated ${fqdn} to resolve to ${wan_ip}."
else
  slog 'IPs match.  Nothing to do!'
fi
