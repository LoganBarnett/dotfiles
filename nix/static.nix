################################################################################
# This file contains static definitions for things I expect to be essentially
# hardcoded but easy to update programmatically.
#
# For example, Signal Desktop frequently gets expired.  I don't want to
# constantly update my nixpkgs references and thus have to do lots of major,
# risky rebuilds.  So I need something that defines the specifics I need for
# `signal-desktop` (the URL and hash), and then a script can go in and update
# those on demand.
################################################################################
{
  signal-desktop-bin = {
    version = "7.81.0";
    hash = "sha256-LWConZwFOXf6LS1tgpkUACQt3s5nhk/exeJadNjxW30=";
  };
}
