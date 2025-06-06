#!/usr/bin/env bash
################################################################################
# Remap keyboard bindings.
#
# This needs to be re-run whenever a keyboard is physically connected to the
# macOS system.  While macOS does have a stated system wherein people can set
# rules for a keyboard that could be plugged in sometime in the future, it
# doesn't seem to actually work.  This could be user error, but the path forward
# is not obvious.  Simply run this again whenever the keyboard is plugged in.
#
# This also has to be run during a restart.
################################################################################
set -euo pipefail

greppipe() {
  grep "$@" || test $? = 1;
}

# See this for a series of key mapping values:
# https://github.com/apple-oss-distributions/IOHIDFamily/blob/3f62fef86e2938e0f362e70eede0a914222c3e27/IOHIDFamily/IOHIDUsageTables.h
# These must be logically ORed with 0x700000000.
key_offset=0x700000000
key_option_left=0xE2 # The docs refer to this as "Alt".
key_command_left=0xE3 # The docs refer to this as "GUI".
# Kept for reference.
# key_capslock=0x39
# key_control_left=0xE0
keycode() {
  key="$1"
  echo -n $(( key_offset | key))
}
keycode_debug() {
  key="$1"
  echo $(( key_offset | key))
}
keycode_debug $key_option_left
# See about integrating this with the built-in system for nix-darwin, because it
# likely handles all of that.  See
# https://apple.stackexchange.com/questions/329085/tilde-and-plus-minus-±-in-wrong-place-on-keyboard/353941#353941
# for an example of a startup-level setting.  However this means the system
# needs to login or finish a clean boot first.  Having a global setting is
# probably better than having a watch-dog process.
#
# When remapping for a specific device, that device loses the global mappings.
# So we have to reapply them manually.  We use a global setting for for
# caps->control.  Swapping option and command is desirable for external
# keyboards (unless they are Apple keyboards).  `hidutil property --help` has a
# lot of useful information on how to go about this.  Warning: If the device
# indicated with --matching isn't present, the hidutil will apply the
# configuration globally and we don't want that.  This will cause the command
# and option keys to be inverted on the onboard laptop keyboard - undesirable.
# We must first detect if our keyboard is present, and then proceed to provide
# mappings if we so choose.  In fact, we should invert for all non-Apple
# keyboards.  Per
# https://www.usb.org/sites/default/files/documents/hut1_12v2.pdf keyboards are
# page ID 07.  I've tried this and 0x07 but they do not work.
# keyboards=$(hidutil list --matching '{"PrimaryUsagePage":7}')
# Use `hidutil property --set '{"UserKeyMapping": []}'` to set.  echo "Move old
# key map file out of the way to fix non-save error in hidutil."  mv --verbose
# ~/Library/Preferences/com.apple.symbolichotkeys.plist{,.old}
external_keyboards=$(
  hidutil list \
    | greppipe --ignore-case keyboard \
    | greppipe --invert-match 'Apple ' \
    | greppipe AppleUserHIDDevice \
    | tr -s ' ' \
    | cut -d $' ' -f 2 \
    | uniq
         )
echo "External keyboards found: $external_keyboards"
echo "Current mappings: $(hidutil property --get UserKeyMapping)"
echo "Clearing..."
# This might seem counterintuitive because we have some other places where
# keymappings are done, and those would be cleared.  However those don't seem to
# step on anything we have in the UI or done to the base keyboard.  Something
# isn't understood here.
hidutil property --set '{"UserKeyMapping": []}'
# This might have been some kind of user error on my part.  Keep an eye on this.
hidutil property --matching "{\"ProductID\":0x1}" --set '{"UserKeyMapping": []}'
hidutil property --matching "{\"ProductID\":0x200}" --set '{"UserKeyMapping": []}'
echo "Cleared!"

key_remap() {
  keyboards="$1"
  type="$2"
  key_maps="$3"
  # When quoting arrays (as is done in the for loop, lest our new Bash give us
  # an error), empty arrays still cause the for loop to execute on a first,
  # empty element.  So just check it manually like we did in the dark ages.
  if [[ "$keyboards" != "" ]]; then
    for keyboard in "${keyboards[@]}"; do
      match="{\"ProductID\":$keyboard}"
      friendly_name="$(
      hidutil list --ndjson --matching "$match" \
        | jq --raw-output '.Product' \
        | uniq
      )"
      echo "Keyboard '$keyboard' is '$friendly_name'."
      echo "Clearing keyboard remaps for $keyboard..."
      hidutil property \
              --matching "$match" \
              --set '{"UserKeyMapping": []}'
      echo "Setting keyboard remaps for $keyboard..."
      # Beware.  If the --matching argument is malformed (such as if
      # $keyboard is "" here), the command will proceed and just hit the
      # global keyboard or something.
      # We might be duplicating the capslock->control binding, since we set a
      # global one later, but there shouldn't be any harm in it.
      hidutil property \
              --matching "$match" \
              --set '{"UserKeyMapping": '"$key_maps"' }'
    done
  else
    echo "No $type keyboards found.  Nothing to do."
  fi
}

key_remap "$external_keyboards" 'external' '
  [
    {
      "HIDKeyboardModifierMappingSrc":'"$(keycode $key_command_left)"',
      "HIDKeyboardModifierMappingDst":'"$(keycode $key_option_left)"'
    },
    {
      "HIDKeyboardModifierMappingSrc":'"$(keycode $key_option_left)"',
      "HIDKeyboardModifierMappingDst":'"$(keycode $key_command_left)"'
    },
    {
      "HIDKeyboardModifierMappingSrc":0x700000039,
      "HIDKeyboardModifierMappingDst":0x7000000E0
    }
  ]
'

internal_keyboards=$(
  hidutil list \
    | greppipe --ignore-case keyboard \
    | greppipe 'Apple ' \
    | greppipe AppleHIDKeyboardEventDriverV2 \
    | tr -s ' ' \
    | cut -d $' ' -f 2 \
    | uniq
)
key_remap "$internal_keyboards" 'internal' '
  [
    {
      "HIDKeyboardModifierMappingSrc":0x700000039,
      "HIDKeyboardModifierMappingDst":0x7000000E0
    }
  ]
'

# echo 'Remapping capslock to control globally...'
# Don't actually run globals of any kind.  In order to completely fix this, I
# need to find the internal keyboard and also remap that separately.
# Global - all keyboards should get this.
# hidutil \
#   property \
#   --set '{"UserKeyMapping":
#     [
#       {
#         "HIDKeyboardModifierMappingSrc":0x700000039,
#         "HIDKeyboardModifierMappingDst":0x7000000E0
#       }
#     ]
#   }'
echo 'Done!'
