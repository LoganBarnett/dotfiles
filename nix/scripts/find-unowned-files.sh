#!/usr/bin/env bash

# Find all files in the git repository that have never been created or edited
# by Logan Barnett.

set -euo pipefail

# Check if we're in a git repository.
if ! git rev-parse --git-dir > /dev/null 2>&1; then
    echo "Error: Not in a git repository" >&2
    exit 1
fi

# Get all tracked files and check git history for each.
git ls-files | while IFS= read -r file; do
    # Check if Logan Barnett (any variation) has any commits for this file.
    # Match: "Logan Barnett", "logan-barnett-<suborg>", "LoganBarnett", etc.
    # Note: Disable pipefail and errexit temporarily. We need to capture the exit
    # code from grep, which may be 0 (match), 1 (no match), or 141 (SIGPIPE).
    set +o pipefail
    set +e
    git log --all --follow --author=".*[Ll]ogan.*" -- "$file" | grep -q .
    has_commits=$?
    set -e
    set -o pipefail

    # If grep didn't find any matches (exit code != 0, excluding SIGPIPE 141),
    # then Logan never touched this file.
    if [ $has_commits -ne 0 ] && [ $has_commits -ne 141 ]; then
        echo "$file"
    fi
done
