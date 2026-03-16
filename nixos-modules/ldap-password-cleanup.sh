#!/usr/bin/env bash

set -euo pipefail

TMDIR=/tmp

echo "${USER:-}"
echo "$(id -ng)"
for user in "$@"; do
  path=/run/agenix/$user-ldap-password-hashed
  sedded="$(sed -E 's/$//' "$path")"
  # Somehow this works better than using --in-place in terms of removing line
  # breaks.  I suppose sed tries to be a good POSIX program and add the line
  # break in, but we actually want it gone.
  echo -n "$sedded" > "$path"
  echo "Fixed $path."
done
echo "Successful."
