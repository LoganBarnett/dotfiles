{
  fetchPypi,
  fetchFromGitHub,
  lib,
  pkgs ? import <nixpkgs> {},
  python3Packages,
}:
python3Packages.buildPythonApplication (let
  pname = "lastversion";
  version = "3.5.5";
in {
  inherit pname version;
  # But also see https://github.com/dvershinin/lastversion.  I should put this
  # in the meta.
  src = fetchPypi {
    inherit pname version;
    hash = "sha256-I2Ego7fLpt+n3q9CC1CUOBk/ulA72XtAtVasrTaTW7w=";
  };
  build-system = [ python3Packages.setuptools ];
  dependencies = [
    python3Packages.requests
    python3Packages.packaging
    python3Packages.cachecontrol
    python3Packages.cachecontrol.passthru.optional-dependencies.filecache
    python3Packages.appdirs
    python3Packages.feedparser
    python3Packages.python-dateutil
    python3Packages.pyyaml
    python3Packages.tqdm
    python3Packages.beautifulsoup4
    python3Packages.distro
    python3Packages.urllib3
  ];
})
