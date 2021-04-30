self: super:
  # inherit (super.callPackage ./nix { pkgs = self; }) openconnect-sso;
{ lib
, openconnect
, python39
, python39Packages
, poetry2nix
, substituteAll
, wrapQtAppsHook
}:

poetry2nix.mkPoetryApplication {
  src = lib.cleanSource ../.;
  pyproject = ../pyproject.toml;
  poetrylock = ../poetry.lock;
  python = python39;
  buildInputs = [ wrapQtAppsHook ];
  propagatedBuildInputs = [ openconnect ];

  dontWrapQtApps = true;
  makeWrapperArgs = [
    "\${qtWrapperArgs[@]}"
  ];

  overrides = [
    poetry2nix.defaultPoetryOverrides
    (
      self: super: {
        inherit (python39Packages) cryptography keyring pyqt5 pyqtwebengine six;
        coverage_enable_subprocess = with python39.pkgs; buildPythonPackage rec {
          pname = "coverage_enable_subprocess";
          version = "1.0";

          src = fetchPypi {
            inherit pname version;
            sha256 = "04f0mhvzkvd74m97bldj9b5hqnsc08b8xww4xy3ws1r0ag4kvggx";
          };

          propagatedBuildInputs = [ super.coverage ];
          doCheck = false;
        };
        pytest-httpserver = super.pytest-httpserver.overrideAttrs (
          old: {
            nativeBuildInputs = old.nativeBuildInputs ++ [ self.pytestrunner ];
          }
        );
      }
    )
  ];
}
