#!/usr/bin/env bash
# Format only staged files using treefmt, leaving unstaged edits alone.
root=$(git rev-parse --show-toplevel)
staged=$(git diff --cached --name-only --diff-filter=d)
if [ -z "$staged" ]; then
  echo "No staged files." >&2
  exit 0
fi
cd "$root" || exit 1
# Pass paths NUL-delimited so names containing spaces are handled correctly.
git diff --cached --name-only --diff-filter=d -z | xargs -0 treefmt
