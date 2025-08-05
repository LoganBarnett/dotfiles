{ lib, fetchFromGitHub, fetchPypi, python3, ... }: let
  pname = "zalgo-cli";
  python = python3;
  version = "0.5.0";
in
  python.pkgs.buildPythonApplication {
    inherit pname version;
    format = "pyproject";
    src = fetchFromGitHub {
      owner = "tddschn";
      repo = "zalgo-cli";
      rev = "v${version}";
      hash = "sha256-UOL0UuuXlFmnV+sBuvTJk/CMnmMVurVjkm9CinEcBUw=";
    };
    nativeBuildInputs = [
      python.pkgs.poetry-core
    ];
    propagatedBuildInputs = [
      python.pkgs.gradio
    ];
    # Looks like the current version isn't published to Pypi.
    # src = fetchPypi {
    #   inherit pname;
    #   version = "v${version}";
    # };
    # This requires an older version of gradio which is not on Nix.  We could do
    # the song and dance to build an older version, but really this package
    # should be made more up to date probably.  We don't care about gradio
    # support (zalgo-gradio is actually a separate package entirely), so let's
    # just disable this check for now.  Unfortunately we must disable the entire
    # check phase from what we can see.
    dontCheckRuntimeDeps = true;
    meta = {
      description = "Zalgo text generator CLI.";
      homepage = "https://github.com/tddschn/zalgo-cli";
      license = lib.licenses.mit;
    };
  }
