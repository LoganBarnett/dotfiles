# There's a series of issues involving gpg:
# 1. gpg is expecting $HOME/.gnupg to be present.  This resolves to
#    /homeless-shelter/.gnupg and it doesn't exist.  Setting $HOME to $(pwd)
#    makes it work again, because the directory is present and it is able to
#    create its predefined directory.
# 2. git-lfs uses gpg encrypted commits during its test (or reads from the
#    global git config that is laid down, which indicates we should sign
#    commits).  This fouls up the tests as those keys might have not be added,
#    or the gpg agent not populated before these tests are run.  This kind of
#    local state should not pollute or contribute to the tests.
# TODO File a fix for git to make it include a standard system config dir.
# TODO Update the documentation on git for NixOS to 1) be correct (it isn't) and
# 2) warn that global settings can interfere with builds (ex: set gpg signing in
# your system config and do a build of pythonPackages.hatch-vcs or git-lfs).
# TODO Update hatch-vcs and git-lfs to set `GIT_CONFIG_NOSYSTEM=true` in their
# checkPhase or something equivalent where the failure occurs.
(final: prev: {

  pythonPackagesExtensions = [(py-final: py-prev: {
    hatch-vcs = py-prev.hatch-vcs.overrideAttrs (old: {
      # checkPhase = "export HOME=$(pwd)";
      checkPhase = ''
        GIT_CONFIG_GLOBAL=/dev/null
        GIT_CONFIG_SYSTEM=/dev/null
        GIT_CONFIG_NOSYSTEM=true
      '';
      # pytestCheckPhase = ''
      #   export HOME=$(pwd)
      # '' + old.pytestCheckPhase;
      nativeBuildInputs = old.nativeBuildInputs ++ [ final.gnupg ];
    });
  })];

  # Running phase: checkPhase
  # ok      github.com/git-lfs/git-lfs/v3/commands  0.035s
  # ok      github.com/git-lfs/git-lfs/v3/config    0.046s
  # ok      github.com/git-lfs/git-lfs/v3/creds     0.008s
  # ok      github.com/git-lfs/git-lfs/v3/errors    0.008s
  # ok      github.com/git-lfs/git-lfs/v3/filepathfilter    0.011s
  # ok      github.com/git-lfs/git-lfs/v3/fs        0.008s
  # --- FAIL: TestCurrentRefAndCurrentRemoteRef (0.09s)
  #     testutils.go:429: Error committing: exit status 128 error: gpg failed to sign the data:
  #         gpg: Fatal: can't create directory '/homeless-shelter/.gnupg': No such file or directory
  #
  #         fatal: failed to write commit object
  # --- FAIL: TestRecentBranches (0.08s)
  #     testutils.go:429: Error committing: exit status 128 error: gpg failed to sign the data:
  #         gpg: Fatal: can't create directory '/homeless-shelter/.gnupg': No such file or directory
  #
  #         fatal: failed to write commit object
  # --- FAIL: TestWorkTrees (0.08s)
  #     testutils.go:429: Error committing: exit status 128 error: gpg failed to sign the data:
  #         gpg: Fatal: can't create directory '/homeless-shelter/.gnupg': No such file or directory
  #
  #         fatal: failed to write commit object
  # --- FAIL: TestGetTrackedFiles (0.11s)
  #     testutils.go:429: Error committing: exit status 128 error: gpg failed to sign the data:
  #         gpg: Fatal: can't create directory '/homeless-shelter/.gnupg': No such file or directory
  #
  #         fatal: failed to write commit object
  # --- FAIL: TestLocalRefs (0.08s)
  #     testutils.go:429: Error committing: exit status 128 error: gpg failed to sign the data:
  #         gpg: Fatal: can't create directory '/homeless-shelter/.gnupg': No such file or directory
  #
  #         fatal: failed to write commit object
  # --- FAIL: TestGetFilesChanges (0.08s)
  #     testutils.go:429: Error committing: exit status 128 error: gpg failed to sign the data:
  #         gpg: Fatal: can't create directory '/homeless-shelter/.gnupg': No such file or directory
  #
  #         fatal: failed to write commit object
  # FAIL
  # FAIL    github.com/git-lfs/git-lfs/v3/git       0.858s
  # FAIL
  git-lfs = prev.git-lfs.overrideAttrs (old: {
    nativeBuildInputs = old.nativeBuildInputs ++ [ final.gnupg ];
    checkPhase = ''
      export HOME=$(pwd)
      GIT_CONFIG_GLOBAL=/dev/null
      GIT_CONFIG_SYSTEM=/dev/null
      GIT_CONFIG_NOSYSTEM=true
      git config --list
    '' + old.checkPhase;
    # checkPhase = null;
  });

})
