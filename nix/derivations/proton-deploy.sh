#!/usr/bin/env bash

set -euo pipefail

# See some quirks to this process here:
# https://discourse.nixos.org/t/remote-nixos-rebuild-works-with-build-but-not-with-switch/34741

# Allow for any verb.  "switch" is typical - it builds and activates the system
# derivation, and also installs the bootloader.  "test" will build and activate
# the system derivation, but won't add it to the bootloader.  This means the
# derivation will be not be loaded on reboot - the prior activation will be used
# instead.  There is also "boot", which just installs the boot loader.
verb="$1"
host="$2"
shift 2

# --fast is the same as --no-build-nix which is "useful you call nixos-rebuild
# frequently" but it also makes the difference between working and not working.
# In this case if `--fast` is omitted, we see:
# /nix/store/f12m00gy5s976qm6p4xmcv8mkvc24an3-nixos-rebuild/bin/nixos-rebuild: line 432: /nix/store/ssqdazslv2wjpybnk0pjwnpq0dfpa6q2-coreutils-9.5/bin/mktemp: cannot execute binary file: Exec format error
# See
# https://discourse.nixos.org/t/deploy-nixos-configurations-on-other-machines/22940/23
# for more oddities on the topic.
nixos-rebuild \
  $verb \
  --flake '.#'"$host" \
  --target-host "$host.proton" \
  --build-host "$host.proton" \
  --use-remote-sudo \
  --fast \
  "$@"
