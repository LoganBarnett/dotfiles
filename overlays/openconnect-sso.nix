# Keep as reference of a Poetry build, though I don't know if this actually
# works.
final: prev:
let
  openconnect-sso-src = builtins.fetchTarball "https://github.com/vlaci/openconnect-sso/archive/master.tar.gz";
in
{
  openconnect-sso = final.poetry2nix.mkPoetryApplication {
    src = final.lib.cleanSource "${openconnect-sso-src}/.";
    pyproject = "${openconnect-sso-src}/pyproject.toml";
    poetrylock = "${openconnect-sso-src}/poetry.lock";
    python = final.python39;
    buildInputs = [ final.libsForQt5.wrapQtAppsHook ];
    propagatedBuildInputs = [ final.openconnect ];

    dontWrapQtApps = true;
    makeWrapperArgs = [
      "\${qtWrapperArgs[@]}"
    ];

    overrides = [
      prev.poetry2nix.defaultPoetryOverrides
      (
        final: prev: {
          inherit (final.pkgs.python39Packages) cryptography keyring pyqt5 pyqtwebengine six;
          coverage_enable_subprocess = with final.python39.pkgs; buildPythonPackage rec {
            pname = "coverage_enable_subprocess";
            version = "1.0";

            src = fetchPypi {
              inherit pname version;
              sha256 = "04f0mhvzkvd74m97bldj9b5hqnsc08b8xww4xy3ws1r0ag4kvggx";
            };

            propagatedBuildInputs = [ prev.coverage ];
            doCheck = false;
          };
          pytest-httpserver = prev.pytest-httpserver.overrideAttrs (
            old: {
              nativeBuildInputs = old.nativeBuildInputs ++ [ final.pytestrunner ];
            }
          );
        }
      )
    ];
  };
}
