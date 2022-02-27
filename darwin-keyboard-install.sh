#!/usr/bin/env bash

# Keyboard settings script found in: https://stackoverflow.com/a/58907582

# Quit System Preferences so it doesn't muck with your settings.
osascript -e 'tell application "System Preferences" to quit'

# Key codes, sans the 7000000xx prefix, can be found here:
# https://gist.github.com/MightyPork/6da26e382a7ad91b5496ee55fdc73db2
# Or here:
# https://developer.apple.com/library/archive/technotes/tn2450/_index.html#//apple_ref/doc/uid/DTS40017618-CH1-KEY_TABLE_USAGES
# I found the latter more useful.
#
# 0x700000039 - caps-lock.
# 0x7000000E4 - right control.
# 0x7000000E0 - left control.
# Remap caps-lock to escape
hidutil property --set '{
  "UserKeyMapping":[{
    "HIDKeyboardModifierMappingSrc":0x700000039,
    "HIDKeyboardModifierMappingDst":0x7000000E4
  }]
}'

# TODO: Document this keyboard keycode usage somewhere on disk.
# TODO: Make this work for external keyboards too. This appears to be global or
# whatever the "main" keyboard is.
