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
    version = "7.85.0";
    hash = "sha256-oXt4MlSvpj4NHwULg8K0XukaMqnlsF2UPkvBGH3Htko=";
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
}
