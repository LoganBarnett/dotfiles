#!/usr/bin/env bash

if [[ ! -n "$BBS_AUTH" ]]; then
  echo 'BBS_AUTH missing' 2>&1
  exit 1
fi

if [[ ! -n "$BBS_BASE_URL" ]]; then
  echo 'BBS_BASE missing' 2>&1
  exit 1
fi

start=0
while :; do
  page=$(
    curl \
      --silent \
      --user "$BBS_AUTH" \
      --basic \
      "$BBS_BASE_URL/rest/api/1.0/repos?limit=100&start=$start" \
  )
  echo "$page" | jq --raw-output '
    .values[]
      | select(.project.key | startswith("~") | not)
      | "\(.project.key),\(.slug)"
    '
    # This could be used instead, but escaping and quoting complicates things.
    # Project keys and repo slugs are restricted in what they have, so we're
    # fine to just use commas.
    # | [ .project.key, .slug ]
    # | @csv
  jq -e '.isLastPage' <<<"$page" >/dev/null && break
  echo "BBS: records -- $(jq --raw-output '.start + .size' <<< "$page") " 2>&1
  start=$(jq -r '.nextPageStart' <<<"$page")
done
echo 'BBS records done!' 2>&1
