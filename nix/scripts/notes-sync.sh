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
    * ) break ;;
  esac
done

if [[ "${sync_dir:-}" == '' ]]; then
  echo 'Error: Missing required argument --sync-dir.  Exiting.'
  exit 1
fi

if [[ "${git_url:-}" == '' ]]; then
  echo 'Error: Missing required argument --git-url.  Exiting.'
  exit 1
fi

if [[ "${ssh_identity:-}" == '' ]]; then
  echo 'Error: Missing required argument --ssh-identity.  Exiting.'
  exit 1
fi

declare -g SSH_AGENT_PID=''

# Shellcheck thinks this is unreachable, but it just isn't detecting the trap
# registration correctly.
# shellcheck disable=SC2317
clean_ssh_agent() {
  kill "$SSH_AGENT_PID"
}

eval "$(ssh-agent)"

trap clean_ssh_agent EXIT

ssh-add "$ssh_identity"
mkdir --parents "$sync_dir"
cd "$sync_dir"

if [ ! -d "$sync_dir/.git" ]; then
  echo "No git repository detected at $sync_dir.  Cloning $git_url into it..."
  # Is this really secure anyways?
  GIT_SSH_COMMAND="ssh -o StrictHostKeyChecking=no" git clone "$git_url" .
# Note: Use -z for the inverted form (positive on no changes present).
elif [ -n "$(git status --short)" ] ; then
  git config user.email "$USER@$HOSTNAME"
  git config user.name "$USER"
  git add .
  time="$(date --iso-8601=ns)"
  git commit -m "Update from WebDAV changes on $time"
  git fetch --all
  # Try to handle merge conflicts by simply accepting what's on the remote.
  # TODO: Some day we should try to rebase instead.
  git merge \
    -s recursive -X theirs \
    -m "Merge from webdave changes on $time." \
    "origin/$(git symbolic-ref --short HEAD)"
  git push
fi
exit 0
