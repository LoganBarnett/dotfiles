################################################################################
# Python CLI for talking to Gree-protocol mini-split heat pumps over the local
# network.  Used to provision units onto WiFi without the Gree+ cloud app and to
# read/set parameters once they are on the LAN.  The upstream repo also ships
# Qt/Android/.NET/ESP8266 implementations; only PythonCLI/gree.py is packaged
# here because it is the only one suited to scripting from a workstation.
#
# Pinned to the AES-GCM commit (Sept 2024), which is required by newer
# firmware revisions of Gree-OEM units (e.g. Stealth Pinnacle).
################################################################################
{
  fetchFromGitHub,
  lib,
  makeWrapper,
  python3,
  stdenv,
}:
let
  pname = "gree-remote";
  version = "unstable-2024-09-12";
  pythonEnv = python3.withPackages (
    ps: with ps; [
      cryptography
      pycryptodome
    ]
  );
in
stdenv.mkDerivation {
  inherit pname version;

  src = fetchFromGitHub {
    owner = "tomikaa87";
    repo = "gree-remote";
    rev = "16962adfc4f41f1d896048299e47c311a326ca96";
    hash = "sha256-6Q6qh7xHhnM4IxdpDJvjFqqHJLAxmJip+XF9mNewny8=";
  };

  nativeBuildInputs = [ makeWrapper ];

  dontBuild = true;

  installPhase = ''
    runHook preInstall

    install -Dm644 PythonCLI/gree.py "$out/share/gree-remote/gree.py"
    makeWrapper ${pythonEnv}/bin/python3 "$out/bin/gree-remote" \
      --add-flags "$out/share/gree-remote/gree.py"

    runHook postInstall
  '';

  meta = {
    description = "Local-network CLI for Gree-protocol air conditioners and heat pumps";
    homepage = "https://github.com/tomikaa87/gree-remote";
    license = lib.licenses.mit;
    mainProgram = "gree-remote";
    platforms = lib.platforms.unix;
  };
}
