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

  claude-code = {
    version = "2.1.59";
    hash = "sha256-Dam9aJ0qBdqU40ACfzGQHuytW6ur0fMLm8D5fIKd1TE=";
    npmDepsHash = "sha256-k2ytvmySVD/qBfIOc5i1D1Ge8D/gmO9kjh4eDN9ceZc=";
  };

  makemkv = {
    version = "1.18.3";
    # MakeMKV has two components: oss (open source) and bin (proprietary).
    oss = {
      hash = "sha256-vIuwhK46q81QPVu5PvwnPgRuT9RmPTmpg2zgwEf+6CM=";
    };
    bin = {
      hash = "sha256-we5yCukbJ2p8ib6GEUbFuTRjGDHo1sj0U0BkNXJOkr0=";
    };
  };

  signal-desktop-bin = {
    version = "7.89.0";
    hash = "sha256-Y2xUCWDRsb+A2GypvxZPc1VChRX3ElgrPOeeu4w4FRU=";
  };

}
