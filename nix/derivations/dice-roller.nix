{
  fetchFromGitHub,
  lib,
  pkg-config,
  rustPlatform,
  ...
}: rustPlatform.buildRustPackage (let
  inherit (lib) licenses maintainers;
  name = "dice-roller";
  version = "v0.1.3";
  src = fetchFromGitHub {
    owner = "pbyrne";
    repo = "roller";
    rev = version;
    hash = "sha256-0YckDRBZpYgPgpXXY6fgIsdgqF2VFl/94pwASuYr5WM=";
  };
in {
  pname = name;
  inherit src version;
  nativeBuildInputs = [
    pkg-config
  ];
  cargoLock = {
    lockFile = "${src}/Cargo.lock";
  };
  meta = {
    description = "Roll dice from the command line.";
    homepage = "https://github.com/pbyrne/roller";
    maintainers = with maintainers; [ logan-barnett ];
    license = licenses.mit;
  };
})
