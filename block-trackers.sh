#!/usr/bin/env bash

set -e
source nix/bash-logging

slog "Writing to /etc/hosts to block trackers and other hostile sites. sudo \
will be used..."

blocks="\
www.googleanalytics.com
www.google-analytics.com
"

slog "Getting list from github.com/disconnectme/disconnect-tracking-protection..."

# blocks+=$(curl \
#   -L \
#   https://raw.githubusercontent.com/disconnectme/disconnect-tracking-protection/master/entities.json \
#   |  jq -r '.entities[].resources[]' \
# )

# At last count, this was up to about 3k, shew!
# slog "Got list. ${echo "$blocks" | wc -l} to block."

for url in $blocks; do
  if ! grep -q $url /etc/hosts; then
    sudo sh -c "echo '0.0.0.0 $url' >> /etc/hosts"
  fi
done
