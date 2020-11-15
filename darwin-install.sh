#!/usr/bin/env bash
set -euo pipefail

# Any macOS specific installation requirements should go here.

dir="$(dirname "${BASH_SOURCE[0]}")"
source $dir/dotfiles-functions.sh

log "Installing XCode..."
xcode-select --install || true
