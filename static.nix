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

  discord = {
    darwin = {
      version = "0.0.384";
      hash = "sha256-vAp991ilLVviievPZHGFuyi/zMMpDoApjnNTGkXYbwo=";
    };
    linux = {
      version = "0.0.132";
      hash = "sha256-DDt/zr+9sfvyPYUMKCXqEsRvk7wZaxbw2eCWlwxcVec=";
    };
  };

  # Claude Code 2.1.x ships as a native binary per platform instead of a
  # Node.js bundle.  Each platform has its own npm package with a single
  # compiled executable.
  claude-code = {
    version = "2.1.114";
    aarch64-darwin.hash = "sha256-Nx0I2PZgoLeI43c5O4rlyPgEQGDom3RO5pemV9V1vqg=";
    x86_64-darwin.hash = "sha256-1tUHaaE4AI8r7W+vS4wCKTH3OjDOxMRtSzyUt+LHhAs=";
    x86_64-linux.hash = "sha256-gejcdjRzKnWsvLzxJLfdjr+PeYdOR9tkCOL8owuJuf8=";
    aarch64-linux.hash = "sha256-atThX6FuIJe0t7pQRd76ZIVCPd+AKfkLl1a48eLglQE=";
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
