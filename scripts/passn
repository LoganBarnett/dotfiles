#!/usr/bin/env bash
# A `pass` invocation that automatically trims the trailing newline.  All
# arguments are forward to `pass`.
# The -z helps ensure a lack of NUL added (see
# https://stackoverflow.com/a/58129173).
pass "$@" | sed -E -z '$ s/\n$//'
