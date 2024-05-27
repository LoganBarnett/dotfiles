#!/usr/bin/env sh

set -euo pipefail

source nix/bash-logging

slog "Setting up pass..."

usage() {
  echo "
Builds a key to be used for the pass utility, if needed. Helps guide the user
through the process so a bunch of visits to the web aren't needed.

Usage:

$0 [--real-name <real-name>] [--email <email>]
"
}

# Incomplete. See https://stackoverflow.com/a/7680682 for how to handle long
# options, which isn't in here yet. But I certainly could use a working example!
# This solution actually looks really good: https://stackoverflow.com/a/5230306
while getopts n:e:h: flag
do
    case "${flag}" in
        n) realName=${OPTARG};;
        e) email=${OPTARG};;
        h) help=true;;
    esac
done

# TODO: Work in progress. Need to accept paramters for real name and email.
# At very least, this script can serve as a helper to get started, and then I
# don't have to piece it together again.
if [[ -f ~/.password-store/.gpg-id ]]; then
  slog "GPG key ID already exists. Nothing to do here."
else
  # See
  # https://www.gnupg.org/documentation/manuals/gnupg/Unattended-GPG-key-generation.html
  # for how to populate the script sent to gpg. What I have is not complete. I
  # still need to set the bits to 4096 and use RSA + RSA.
  cat <<EOF | gpg --generate-full-key --batch
Name-Real: $USER
Name-Comment: main-key
Name-Email: email
%commit
%echo Done!
EOF
  slog "Listing signatures. Paste the desired key from the hex string before \
the date. That is the key ID."
  gpg --list-signatures
  echo "Key ID:"
  read keyId
  pass init $keyId
fi
