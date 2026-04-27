################################################################################
# Python CLI for talking to Gree-protocol mini-split heat pumps over the local
# network.  Used to provision units onto WiFi without the Gree+ cloud app and to
# read/set parameters once they are on the LAN.  The upstream repo also ships
# Qt/Android/.NET/ESP8266 implementations; only PythonCLI/gree.py is packaged
# here because it is the only one suited to scripting from a workstation.
#
# Pinned to our fork at LoganBarnett/gree-remote, branch wlan-subcommand,
# which adds the `wlan` and `bind` subcommands missing from upstream.  The
# branch is rebased on master at the AES-GCM commit (Sept 2024), required by
# newer firmware revisions of Gree-OEM units (e.g. Stealth Pinnacle).
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
  version = "unstable-2026-04-27";
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
    owner = "LoganBarnett";
    repo = "gree-remote";
    rev = "7ee5495e8185ee1b0678e9826b666117f2c148bf";
    hash = "sha256-hLJLUNCeDrsaz1akI+mhjKg+bMDCG/ATjWWHfEgnd/I=";
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
    homepage = "https://github.com/LoganBarnett/gree-remote";
    license = lib.licenses.gpl3Plus;
    mainProgram = "gree-remote";
    platforms = lib.platforms.unix;
  };
}
