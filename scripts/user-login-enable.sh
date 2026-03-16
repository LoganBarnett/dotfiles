#!/usr/bin/env bash

for user in "$@"; do
  usermod --expiredate -1 "$user"
done
