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
# Gets a token from the Matrix service and writes it to the token file, so the
# matrix-alertmanager service can pick this value up via its tokenFile setting.
################################################################################

set -euo pipefail

while true; do
  case "${1:-}" in
    -h | --help)
      cat <<EOH
Usege: $0 [opts]

  --password-file <path>     Path to the password file for authentication.
  --token-file    <path>     The token file to write to.
  --username      <username> The user to authenticate as.

Gets a token from the Matrix service and writes it to the token file, so the
matrix-alertmanager service can pick this value up via its tokenFile setting.

EOH
      exit
      ;;
   --password-file)
     password_file="${2:-}"
     shift 2
     ;;
   --token-file)
     token_file="${2:-}"
     shift 2
     ;;
   --username)
     username="${2:-}"
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
missing_var 'password_file'
missing_var 'token_file'
missing_var 'username'

if [[ \
    $password_file_missing == true \
    || $token_file_missing == true \
    || $username_missing == true \
]]; then
  echo "The follow required variables are missing:
--password-file: $config_file_missing
--token-file:    $token_file_missing
--username:      $username_missing
" >&2
  exit 1
fi

# Read the password securely.
password="$(< "$password_file")"
data='{
  "type": "m.login.password",
  "identifier": {
    "type": "m.id.user",
    "user": "'"$username"'"
  },
  "password": "'"$password"'"
}'
# Request login and extract access token.
response=$(
  curl \
    --silent \
    --show-error \
    --request POST \
    "$homeserver_url/_matrix/client/r0/login" \
    --header "Content-Type: application/json" \
    --data "$data"
)

# Check for login failure.
if [[ "$response" == *"M_FORBIDDEN"* ]]; then
  echo "Login failed: Forbidden" >&2
  exit 1
fi

# Extract the access_token.
access_token=$(echo "$response" | jq -r '.access_token')

# Sanity check
if [[ -z "$access_token" || "$access_token" == "null" ]]; then
  echo "Error: Access token not found in response." >&2
  echo "$response" >&2
  exit 1
fi

# Save the token securely.
mkdir --parents "$(dirname "$token_file")"
echo "$access_token" > "$token_file"
chmod 0440 "$token_file"

echo "✅ Token refreshed and stored at $token_file." >&2
