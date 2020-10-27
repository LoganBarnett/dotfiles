#!/usr/bin/env bash
set -euo pipefail

sudo sh -c '
security find-certificate -a -p \
  /Library/Keychains/System.keychain > \
  /nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt ; \
security find-certificate -a -p \
  /System/Library/Keychains/SystemRootCertificates.keychain >> \
  /nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt'
