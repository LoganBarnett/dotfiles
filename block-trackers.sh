#!/usr/bin/env bash

set -e
source bash-logging

slog "Writing to /etc/hosts to block trackers and other hostile sites. sudo \
will be used..."

blocks="\
www.googleanalytics.com
www.google-analytics.com
"

for url in $blocks; do
  if ! grep -q $url /etc/hosts; then
    sudo sh -c "echo '0.0.0.0 $url' >> /etc/hosts"
  fi
done
