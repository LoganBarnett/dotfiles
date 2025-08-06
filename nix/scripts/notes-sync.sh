#!/usr/bin/env bash
set -euo pipefail

sync_dir=''
while true; do
  case "${1:-}" in
    --git-url)
      git_url="${2:-}"
      echo "set git_url to $git_url"
      shift 2
      ;;
    --ssh-identity)
      ssh_identity="${2:-}"
      echo "set ssh_identity to $ssh_identity"
      shift 2
      ;;
    --sync-dir)
      sync_dir="${2:-}"
      echo "set sync_dir to $sync_dir"
      shift 2
      ;;
    --relative-notes-dir)
      relative_notes_dir="${2:-}"
      echo "set relative_notes_dir to $relative_notes_dir"
      shift 2
      ;;
    * ) break ;;
  esac
done

# Could fail if we haven't cloned yet.
cd $sync_dir || true

# Could fail if we haven't cloned yet.
original_head="$(git rev-parse --verify HEAD || true)"
repo-sync \
  --git-url "${git_url}" \
  --ssh-identity "${ssh_identity}" \
  --sync-dir "${sync_dir}"

# Go there again, because if this is a fresh start (repo wasn't cloned at start
# of run), then we never got in there and we need to try again.
cd $sync_dir
new_head="$(git rev-parse --verify HEAD)"
echo "$original_head == $new_head"
if [[ "$original_head" != "$new_head" ]]; then
  echo "Changes detected.  Running scan on ${relative_notes_dir}..."
  nextcloud-occ files:scan --path="${relative_notes_dir}"
else
  echo "No changes detected.  Skipping NextCloud sync."
fi

exit 0
