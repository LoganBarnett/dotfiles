#!/usr/bin/env bash

set -euo pipefail

 # Our systems do not trust the $WORK certificates yet.
export LDAPTLS_REQCERT='try'
search="first.last@$ORG_DOMAIN"; password=''; ldapsearch \
  -x \
  -D "$USER@$ORG_DOMAIN" \
  -w "${password}" \
  -H ldaps://pubedu.hegn.us \
  -b "DC=pubedu,DC=hegn,DC=us" \
  -o ldif-wrap=no \
  "(&(objectCategory=person)(objectClass=user)(|(sAMAccountName=$search)(mail=$search))" \
  "manager"
# Then query again using the username to get their email,
# which can be used to find them in Jira.
manager='sAMAccountNameFromFirstSearch'; password=''; ldapsearch \
  -x \
  -D "$USER@$ORG_DOMAIN" \
  -w "${password}" \
  -H ldaps://pubedu.hegn.us \
  -b "DC=pubedu,DC=hegn,DC=us" \
  -o ldif-wrap=no \
  "(&(objectCategory=person)(objectClass=user)(sAMAccountName=$manager)" \
  "mail"
