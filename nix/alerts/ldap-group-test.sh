#!/usr/bin/env bash
################################################################################
# Test if our LDAP group membership settings are okay.
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
group_to_find='3d-printers'

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

echo "Zeroth, check if the overlays are even loaded."

ldapsearch \
  -H "ldaps://$host:$port" \
  -b "" \
  -D "$bind_dn" \
  -w "$bind_password" \
  -x \
  -v \
  -s base \
  'objectClass=olcOverlayConfig' \
  '*'

ldapsearch \
  -H "ldaps://$host:$port" \
  -b 'cn=config' \
  -D "$bind_dn" \
  -w "$bind_password" \
  -x \
  -v \
  'objectClass=olcModuleList' \
  'olcModuleLoad' || true

echo "First, find the group: '$group_to_find'"
ldapsearch \
  -H "ldaps://$host:$port" \
  -b "$base_dn" \
  -D "$bind_dn" \
  -w "$bind_password" \
  -x \
  -v \
  "cn=$group_to_find"

echo "Next, find all members of the group: '$group_to_find'"

ldapsearch \
  -H "ldaps://$host:$port" \
  -b "$base_dn" \
  -D "$bind_dn" \
  -w "$bind_password" \
  -x \
  -v \
  "memberOf=cn=$group_to_find"

echo "Next, see if we can find the user itself ($user_to_find)."
ldapsearch \
  -H "ldaps://$host:$port" \
  -b "$base_dn" \
  -D "$bind_dn" \
  -w "$bind_password" \
  -x \
  -v \
  "uid=$user_to_find"

echo "Next, see if we can list a user's ($user_to_find) group membership."

ldapsearch \
  -H "ldaps://$host:$port" \
  -b "$base_dn" \
  -D "$bind_dn" \
  -w "$bind_password" \
  -x \
  -v \
  uid=$user_to_find memberOf

echo "Finally, find a specific member ($user_to_find) in the group: '$group_to_find'"

ldapsearch \
  -H "ldaps://$host:$port" \
  -b "$base_dn" \
  -D "$bind_dn" \
  -w "$bind_password" \
  -x \
  -v \
  "(&(uid=$user_to_find)(memberOf=cn=$group_to_find))"
  # "(&(uid=$user_to_find)(memberOf=cn=$group_to_find,ou=users,dc=proton,dc=org))"
  # "(&(ou=$group_to_find)(memberof=uid=$user_to_find,ou=users,dc=proton,dc=org))"
  # "(memberOf=cn=$group_to_find)"
  # "(&(objectClass=inetOrgPerson)(uid=$user_to_find)(memberof=cn=$group_to_find,ou=groups,dc=proton.dc=org))"
