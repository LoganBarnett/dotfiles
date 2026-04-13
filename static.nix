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

  # MakeMKV 1.18.x has a firmware flashing bug (100% CPU spin).  This older
  # binary is used solely for the `makemkvcon f` firmware tool.
  makemkv-flasher = {
    version = "1.17.7";
    bin = {
      hash = "sha256-jFvIMbyVKx+HPMhFDGTjktsLJHm2JtGA8P/JZWaJUdA=";
    };
  };

  signal-desktop-bin = {
    version = "8.6.0";
    hash = "sha256-MruUd6ClCgAWasgluYMYaBI9tavYpmnTNU9kjH85g+c=";
  };

  bgutil-pot = {
    version = "0.8.1";
    x86_64-linux = {
      hash = "sha256-58JkpXT6JwW25dxiKDqKToATDye51+nfROawmqYVGoc=";
    };
    plugin = {
      hash = "sha256-mf2DuY+pOxk9ajtp3HRBDXbnoriJhoxU0WEhyskGA0Q=";
    };
  };

  firefox-bin = {
    version = "148.0.2";
    hash = "sha256-h9yQ8JySvc3Jl402L8Q/zF2Ltyf0nxPy43k65wCNoTI=";
  };

  yt-dlp = {
    version = "2026.03.17";
    hash = "sha256-A4LUCuKCjpVAOJ8jNoYaC3mRCiKH0/wtcsle0YfZyTA=";
  };

  zoom-us = {
    version = "7.0.0.77593";
    hash = "sha256-YSUaM8YAJHigm4M9W34/bD164M8f/hbhtcmHyUwFN20=";
  };

}
