self: super:
# let
#   openconnect-sso-src = builtins.fetchTarball "https://github.com/vlaci/openconnect-sso/archive/master.tar.gz";
# in
{
#   openconnect-sso = self.poetry2nix.mkPoetryApplication {
#     src = self.lib.cleanSource "${openconnect-sso-src}/.";
#     pyproject = "${openconnect-sso-src}/pyproject.toml";
#     poetrylock = "${openconnect-sso-src}/poetry.lock";
#     python = self.python39;
#     buildInputs = [ self.libsForQt5.wrapQtAppsHook ];
#     propagatedBuildInputs = [ self.openconnect ];

#     dontWrapQtApps = true;
#     makeWrapperArgs = [
#       "\${qtWrapperArgs[@]}"
#     ];

#     overrides = [
#       super.poetry2nix.defaultPoetryOverrides
#       (
#         self: super: {
#           inherit (self.pkgs.python39Packages) cryptography keyring pyqt5 pyqtwebengine six;
#           coverage_enable_subprocess = with self.python39.pkgs; buildPythonPackage rec {
#             pname = "coverage_enable_subprocess";
#             version = "1.0";

#             src = fetchPypi {
#               inherit pname version;
#               sha256 = "04f0mhvzkvd74m97bldj9b5hqnsc08b8xww4xy3ws1r0ag4kvggx";
#             };

#             propagatedBuildInputs = [ super.coverage ];
#             doCheck = false;
#           };
#           pytest-httpserver = super.pytest-httpserver.overrideAttrs (
#             old: {
#               nativeBuildInputs = old.nativeBuildInputs ++ [ self.pytestrunner ];
#             }
#           );
#         }
#       )
#     ];
#   };
}
