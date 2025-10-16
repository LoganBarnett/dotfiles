#!/usr/bin/env bash
set -euo pipefail

EXECUTE="false"
SUDO=""
if [ "${EUID:-$(id -u)}" -ne 0 ]; then
  SUDO="sudo"
fi

# Parse args
while true; do
  arg="${1:-}"
  case "$arg" in
    --execute)
      EXECUTE="true"
      shift 1
      ;;
    --help|-h)
      cat <<'USAGE'
Usage: nix-break-and-delete-locks.sh [--execute]

Without --execute: print what would happen.
With    --execute: run `nix store break-lock` and then `nix store delete`.

Notes:
- Only considers top-level /nix/store/*.lock files (no recursion).
- Uses `nix store delete --recursive --ignore-liveness` if available,
  otherwise falls back to `nix-store --delete`.
USAGE
      exit 0
      ;;
    # *) echo "Unknown argument: '$arg'" >&2; exit 2 ;;
    *) break ;;
  esac
done

# Helper: delete a store path using modern or legacy command.
delete_path() {
  local path="$1"
  if nix store delete --help >/dev/null 2>&1; then
    # --recursive: also delete refs; --ignore-liveness: ignore GC roots.
    # Dangerous, but often needed).
    $SUDO nix store delete --recursive --ignore-liveness -- "$path" || true
  else
    # Legacy nix-store fallback
    $SUDO nix-store --delete -- "$path" || true
  fi
}

# Find only top-level *.lock entries in /nix/store (portable: uses -prune instead of -maxdepth)
# Explanation:
#   -type d ! -path /nix/store -prune : prune all subdirectories so we never
#   descend.
#   -o -type f -name '*.lock' -print0 : then print matching files at the top
#   level.
lock_stream() {
  find /nix/store -type d ! -path /nix/store -prune -o -type f -name '*.lock' -print0
}

found_any="false"

# Read NUL-separated paths safely
while IFS= read -r -d "" lock_path; do
  found_any="true"
  drv_path="${lock_path%.lock}"

  if [ "$EXECUTE" = "true" ]; then
    echo "[break] $drv_path"
    # Break the lock on the derivation.
    $SUDO nix store break-lock -- "$drv_path"
    # Delete the derivation (and, recursively, refs if supported).
    echo "[delete] $drv_path"
    delete_path "$drv_path"
  else
    echo "[would break] $drv_path"
    echo "[would delete] $drv_path"
  fi
done < <(lock_stream)

if [ "$found_any" = "false" ]; then
  echo "No /nix/store/*.lock files found at top level."
fi
