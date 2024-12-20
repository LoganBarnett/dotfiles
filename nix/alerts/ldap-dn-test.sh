#!/usr/bin/env bash
################################################################################
# Test if our LDAP settings are okay.
################################################################################
set -euo pipefail

debug_level='-1'
host='nickel.proton'
port='636'
base_dn='dc=proton,dc=org'
# For the uninitiated to LDAP, this is effectively a username to log in as, but
# it uses LDAP itself to query the object that grants the permissions needed.
# TODO: Create an alert service account for this purpose and use that.
# bind_dn='cn=admin,dc=proton,dc=org'
bind_dn='uid=logan,ou=users,dc=proton,dc=org'
# bind_dn='admin@proton.org'
user_to_find='logan'

if [[ "${bind_password:-}" == '' ]]; then
  echo 'Error: bind_password environment variable must be set for non-interactive use.'
  exit 1
fi

# Per https://serverfault.com/a/818746 we can't test s_client with -starttls,
# but may have at one point.  Instead, use ldapsearch's -ZZ parameter to require
# TLS negotiation over starttls.  The rest is left for reference, in case I
# think this is a good idea again.
# echo "First off, make sure that there's a TLS certificate in use..."
# openssl s_client -showcerts -connect "$host:$port"
# echo "Certificates look good.  Trying actual `ldapsearch` invocation..."

# ldapsearch has a difference between -h + -ZZ vs. -H using ldaps for TLS.  They
# are very different beasts from what I've gathered on the explanation here:
# https://www.openldap.org/lists/openldap-software/200402/msg00335.html
# Use -H with ldaps to use TLS the way it was meant to be.
# Don't use debug_level unless you really mean to.
# -d "$debug_level" \
# This doesn't seem to be working...?
# -d 63 \
set -x
ldapsearch \
  -H "ldaps://$host:$port" \
  -b "$base_dn" \
  -D "$bind_dn" \
  -w "$bind_password" \
  -x \
  -v \
  "uid=$user_to_find"
