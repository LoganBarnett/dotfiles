#!/usr/bin/env bash
set -euo pipefail
cd /tank/data
btrfs subvolume list . \
  | awk '{print substr($0, index($0,$9))}' \
  | xargs -I{} btrfs subvolume delete {}
# These should be empty now.  Anything else should alert us.
rmdir snapshots/*
