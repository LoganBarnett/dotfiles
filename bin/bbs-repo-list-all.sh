#!/usr/bin/env bash

start=0
while :; do
  page=$(curl -s -u "$BBS_AUTH" \
    "$BBS_BASE/rest/api/1.0/repos?limit=100&start=$start")
  echo "$page" | jq -r '
    .values[] |
      {project: .project.key,
       repo: .slug,
       ssh:  (.links.clone[]|select(.name=="ssh").href),
       https:(.links.clone[]|select(.name=="http").href)}'
  jq -e '.isLastPage' <<<"$page" >/dev/null && break
  start=$(jq -r '.nextPageStart' <<<"$page")
done
