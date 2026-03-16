{ lib
, stdenv
, rustPlatform
, fetchFromGitHub
, pkg-config
, openssl
, alsa-lib
, pulseaudio
, darwin
}:

let
  pname = "musicgpt";
  version = "unstable-2025-08-17";
  src = fetchFromGitHub {
    owner = "gabotechs";
    repo  = "MusicGPT";
    rev   = "v0.3.28";
    hash  = "sha256-QKx9JgN1l9o8iifKLP3aVzhWO1qVsrv7vm77ItXiHLo=";
  };
  cargoHash = "sha256-7dfpuo9vSLnHeqjDrcAlVIy9mPZ1pd0J9lNP9cNvTkQ=";
  linuxInputs = [
    alsa-lib
    pulseaudio
  ];
in
rustPlatform.buildRustPackage {
  inherit pname version src cargoHash;
  nativeBuildInputs = [
    pkg-config
  ];
  buildInputs = [
    openssl
  ]
    ++ lib.optionals stdenv.isLinux linuxInputs
  ;
  # Force using system OpenSSL via pkg-config (no vendored build).
  OPENSSL_NO_VENDOR = "1";
  OPENSSL_LIB_DIR = "${openssl.out}/lib";
  OPENSSL_INCLUDE_DIR = "${openssl.dev}/include";
  checkFlags = [
    # Ignored because it requires network access.
    "--skip=storage_ext::tests::downloads_remote_file"
  ];

  # Example if upstream has optional features; pin for reproducibility.
  # cargoBuildFlags = [
  #   "--features" "cli,local-inference"
  #   # "--no-default-features" # disables default cargo features
  # ];

  # Keep builds offline & pure. Models should be fetched at runtime.
  # doCheck = false; # uncomment if upstream tests are flaky or net-bound

  meta = with lib; {
    description = ''
      Prompt-to-music (text-to-audio) CLI/app (MusicGen-style) in Rust
    '';
    longDescription = ''
      Packages the MusicGPT Rust application.
      Note: default MusicGen weights are typically CC-BY-NC 4.0
      (non-commercial).  The code may be MIT/Apache, but bundled or fetched
      weights may not be.
    '';
    homepage = "https://github.com/gabotechs/MusicGPT";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
    platforms = platforms.unix;
    mainProgram = "musicgpt";
  };
}
