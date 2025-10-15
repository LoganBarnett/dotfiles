#!/usr/bin/env bash

set -euo pipefail

# "log" is taken, so this is slog - script log. Use it for logging in scripts.
# It prints the name of the script in a decorative means, and as a bonus prints
# the log to stderr. This helps reserve stdout for parseable output.
function slog {
  printf "\e[93m[\e[96m%s\e[93m]\e[0m " "$(basename "$0")" 1>&2
  echo -e "$*" 1>&2
}
# zsh does not like this kind of exporting.
if [[ ! $SHELL =~ "zsh" ]]; then
  export -f slog
fi

mkdir --parents ~/.bbs2gh/
# Assumes Bash.  See https://stackoverflow.com/a/1482133 for possible
# alternatives, and this solution itself.
dir="$(dirname -- "$( readlink -f -- "$0"; )")"

source ~/.config/gh/bbs-env
export BBS_AUTH="$BBS_USERNAME:$BBS_PASSWORD"
slog "Getting all repositories in Bitbucket Server..."
"$dir/bbs-repo-list-all.sh" > ~/.bbs2gh/repos.csv

slog "Found $(cat ~/.bbs2gh/repos.csv | wc -l) repositories on Bitbucket Server."

# We want to skip over any repositories whose migration is completed.  To do
# that, we need to find the repository and inspect its log for the text
# "Migration complete".
# Utterly depressing fact:  `gh repo list` does not paginate, which makes it
# unreliable for basically anything.  See
# https://github.com/cli/cli/discussions/7010 to lose all hope.  Fortunately you
# can just use `gh api` instead with `--paginate`.
mapfile -t remote_repositories < <( \
  gh api /orgs/$GH_ORG/repos --jq '.[].name' --paginate
)

slog "Found $(echo "$remote_repositories" | wc -l) repositories on the target."

function is_repo_migration_complete() {
  target_repo="$1"
  set -x
  gh api /repos/$GH_ORG/$target_repo/issues
  issue_number="$(
    gh api /repos/$GH_ORG/$target_repo/issues \
      --jq '.[] | select(.title == "Migration Log") | .number'
  )"
  if [[ "$issue_number" == "" ]]; then
    slog "$repo has not been completely migrated yet."
    return 1
  fi
  body="$(gh api \
     "/repos/$GH_ORG/$target_repo/issues/$issue_number/comments" \
     --jq '.[].body'
  )"
  if [[ "$body" =~ "Migration complete" ]]; then
    slog "$repo has been completely migrated already."
    return 0
  else
    slog "$repo has not been completely migrated yet."
    return 1
  fi
}

function complete_repositories() {
  for repo in "${remote_repositories[@]}"; do
    if is_repo_migration_complete "$repo" ; then
      echo "$repo"
    fi
  done
}

function filter_complete() {
  mapfile -t migrated_repositories < <( complete_repositories )
  for entry in "${remote_repositories[@]}"; do
    if ! printf '%s\n' "${migrated_repositories[@]}" | grep --quiet "$entry"; then
      echo "$entry"
    fi
  done
}

mapfile -t incomplete_repositories < <( filter_complete )
exit 1

slog "There are $(echo "$incomplete_repositories" | wc -l) repositories to migrate."

# Migration jobs, by default, are limited to 2.  Set `migration.threadpool.size`
# in `$BITBUCKET_HOME/shared/bitbucket.properties` to adjust this value, but it
# requires a server restart, and it might be under your Puppet management.
rush \
  --jobs 10 \
  --keep-order \
  --continue \
  --succ-cmd-file bbs2gh.rush-continue \
  --infile ~/.bbs2gh/repos.csv \
  --field-delimiter ',' \
  --verbose \
  'gh-migrate-bbs-repo {1} {2}'
