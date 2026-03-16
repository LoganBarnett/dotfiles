#!/usr/bin/env bash
################################################################################
# Open a macOS .app bundle by short name, detached from the shell.
#
# Usage: app <name>
# Example: app element, app openscad, app prusa-slicer
#
# The name is matched case-insensitively with hyphens and spaces stripped, so
# "prusa-slicer" matches "PrusaSlicer.app" and "element" matches "Element.app".
# Nix-managed apps are searched before standard macOS locations.
#
# The macOS `open` command launches the app through Launch Services, which
# detaches it from the shell.  The app keeps running if the shell is closed.
################################################################################

if [[ $# -eq 0 ]]; then
  echo "Usage: app <name>" >&2
  exit 1
fi

query="$1"
# Strip hyphens and spaces, then lowercase for fuzzy matching.
query_normal="${query//[- ]/}"
query_normal="${query_normal,,}"

search_dirs=(
  # Nix-darwin system packages.
  "/run/current-system/Applications"
  # Home Manager packages.
  "$HOME/Applications/Home Manager Apps"
  # Standard macOS locations.
  "/Applications"
  "/System/Applications"
  "/System/Applications/Utilities"
)

for dir in "${search_dirs[@]}"; do
  [[ -d "$dir" ]] || continue
  for app in "$dir"/*.app; do
    [[ -e "$app" ]] || continue
    basename="${app##*/}"
    name="${basename%.app}"
    name_normal="${name//[- ]/}"
    name_normal="${name_normal,,}"
    if [[ "$name_normal" == "$query_normal" ]]; then
      open "$app"
      exit 0
    fi
  done
done

echo "app: no app found matching '$1'" >&2
exit 1
