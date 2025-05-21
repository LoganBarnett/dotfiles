{
  fetchFromGitHub,
  lib,
  openssl,
  pkg-config,
  rustPlatform,
  ...
}: rustPlatform.buildRustPackage (let
  inherit (lib) licenses maintainers;
  name = "dness";
  version = "v0.5.7";
  src = fetchFromGitHub {
    owner = "nickbabcock";
    repo = "dness";
    rev = version;
    hash = "sha256-Vty4ec6aoUh3p2b9vLkNeS5R4pJWzjwYrC5DtVVyhT8=";
  };
in {
  pname = name;
  inherit src version;
  nativeBuildInputs = [
    pkg-config
  ];
  PKG_CONFIG_PATH = "${openssl.dev}/lib/pkgconfig";
  cargoLock = {
    lockFile = "${src}/Cargo.lock";
  };
  # Many tests require network access.
  doCheck = false;
  meta = {
    description = "A dynamic DNS updating tool supporting a variety of providers.";
    homepage = "https://github.com/nickbabcock/dness";
    maintainers = with maintainers; [ logan-barnett ];
    license = licenses.mit;
  };
})
