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
  # src = fetchFromGitHub {
  #   owner = "dvershinin";
  #   repo = pname;
  #   rev = "v${version}";
  #   hash = "";
  # };
  src = fetchPypi {
    inherit pname version;
    hash = "";
  };
  build-system = [ python3Packages.setuptools ];
})
