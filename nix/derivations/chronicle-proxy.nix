{ lib
, rustPlatform
, fetchFromGitHub
, pkg-config
, openssl
, stdenv
}:

let
  pname   = "chronicle-proxy";
  version = "0.4.3";
in
rustPlatform.buildRustPackage {
  inherit pname version;

  src = fetchFromGitHub {
    owner = "dimfeld";
    repo  = "chronicle";
    tag   = "chronicle-proxy-v${version}";
    hash  = "sha256-JqRFfynTkba9mVxK703VdNO+32IbcBUxemyXW7MNRdE=";
  };
  cargoHash = "sha256-Dk39IPFjMJq+hSZBUBYydZlVOHUFtCBUI2n4iYDex8Q=";

  # Build only the API crate in the workspace.
  cargoBuildFlags = [
    "--package" "chronicle-api"
    "--locked"
  ];

  SQLX_OFFLINE = "true";

  nativeBuildInputs = [ pkg-config ];
  buildInputs       = [ openssl ];

  doCheck = false;

  meta = with lib; {
    description = "OpenAI-compatible LLM proxy with persistence and observability";
    homepage    = "https://github.com/dimfeld/chronicle";
    license     = licenses.asl20;
    mainProgram = "chronicle";
  };
}
