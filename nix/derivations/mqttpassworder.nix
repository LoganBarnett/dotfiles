{ lib
, fetchFromGitHub
, buildGoModule
}:

let
  version = "0.1.0";
in
buildGoModule {
  pname = "mqttpassworder";
  inherit version;

  src = fetchFromGitHub {
    owner = "shantanoo-desai";
    repo = "mqttpassworder";
    rev = "v${version}";
    hash = "sha256-IAGs6928HynmvK0olYXRQR4qUIq2/3rKkd1p1/QnM/w=";
  };

  vendorHash = "sha256-Ip62iVp+cCbXXreI7L0i6nh2x3S7HFpZcjCXg25o1GU=";

  doCheck = false;

  meta = {
    description = "CLI tool to generate Mosquitto-compatible hashed passwords.";
    homepage = "https://github.com/shantanoo-desai/mqttpassworder";
    license = lib.licenses.gpl3Plus;
    maintainers = [];
  };
}
