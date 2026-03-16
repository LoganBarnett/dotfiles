################################################################################
# Provide gaming capabilities using Android APKs.
#
# My reading done on this so far:
# - Anbox is an open source solution to high performance Android emulation, but
#   it's no longer maintained: https://github.com/anbox/anbox
# - Andy has some research/accusation done towards cryptocurrency miners it
#   installs on your machine.  The installer itself doesn't install it, but
#   instead the miner is stalled as a post-install activity via a server
#   download.  This is incredibly damning and I'll steer clear of that.  Source:
#   https://www.reddit.com/r/emulators/comments/8rj8g5/warning_andy_android_emulator_andyos_andyroid/
# - Bluestacks is on record of being legit
#   (https://www.reddit.com/r/emulators/comments/8rj8g5/comment/e0vnleu/,
#   https://www.reddit.com/r/emulators/comments/8rj8g5/comment/e1az7yi/).  It
#   is, however, closed source.  So it could still be doing anything.  Their
#   business model is a subscription to enable features.  I'd prefer to stay
#   away from subscriptions of any kind.
# - Waydroid is open source, and includes not only a Nix package but also a
#   NixOS module for it.  I have settled on Waydroid for now.  See:
#   https://waydro.id
################################################################################
{ ... }: {
  # That's it!
  virtualisation.waydroid.enable = true;
}
