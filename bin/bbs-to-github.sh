#!/usr/bin/env bash

mkdir --parents ~/.bbs2gh/
# Assumes Bash.  See https://stackoverflow.com/a/1482133 for possible
# alternatives, and this solution itself.
dir="$(dirname -- "$( readlink -f -- "$0"; )")"

source ~/.config/gh/bbs-env
export BBS_AUTH="$BBS_USERNAME:$BBS_PASSWORD"
"$dir/bbs-repo-list-all.sh" > ~/.bbs2gh/repos.csv


# Migration jobs, by default, are limited to 2.  Set `migration.threadpool.size`
# in `$BITBUCKET_HOME/shared/bitbucket.properties` to adjust this value, but it
# requires a server restart, and it might be under your Puppet management.
rush \
  --jobs 2 \
  --keep-order \
  --continue \
  --succ-cmd-file bbs2gh.rush-continue \
  --infile ~/.bbs2gh/repos.csv \
  --field-delimiter ',' \
  --verbose \
  'gh-migrate-bbs {1} {2}'
