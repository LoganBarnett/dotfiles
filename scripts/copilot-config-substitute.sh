#!/usr/bin/env bash
perl -pe '
  s/<%(\w+)%>/exists $ENV{$1} ? $ENV{$1} : "<%$1%>"/ge
' "$HOME/.copilot/mcp-config-template.json" > \
    "$HOME/.copilot/mcp-config.json"
