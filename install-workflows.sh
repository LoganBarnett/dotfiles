#!/usr/bin/env bash

echo "installing alfred workflows"
WORKFLOWS="
top-processes=https://github.com/zhaocai/alfred2-top-workflow/raw/c2e848eff19a4f93b8302e199bf9f353012c3564/Top%20Processes.alfredworkflow
dnd=https://github.com/paulrudy/alfred-toggle-do-not-disturb/raw/cdf4b5d773a36180a471d7d0ca66751478ff571c/toggle-do-not-disturb.alfredworkflow
"

WORKFLOW_DIR=$HOME'/Library/Application Support/Alfred 4/Alfred.alfredpreferences/workflows'

for w in $WORKFLOWS
do
    set -- `echo $w | tr '=' ' '`
    name=$1
    url=$2
    echo "installing workflow \"$name\""
    mkdir -p "$WORKFLOW_DIR/$name"
    cd "$WORKFLOW_DIR/$name"
    wget -qO- "$url" | bsdtar -xf-
done

echo "done installing workflows"
