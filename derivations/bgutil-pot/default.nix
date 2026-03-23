{
  lib,
  autoPatchelfHook,
  fetchurl,
  openssl,
  stdenv,
}:
let
  statics = (import ../../static.nix).bgutil-pot;
  inherit (statics) version;
  platform =
    statics.${stdenv.hostPlatform.system}
      or (throw "bgutil-pot: unsupported platform ${stdenv.hostPlatform.system}");
  archMap = {
    "x86_64-linux" = "x86_64";
    "aarch64-linux" = "aarch64";
  };
  arch =
    archMap.${stdenv.hostPlatform.system} or (throw "bgutil-pot: unsupported arch");
in
stdenv.mkDerivation {
  pname = "bgutil-pot";
  inherit version;

  src = fetchurl {
    url = "https://github.com/jim60105/bgutil-ytdlp-pot-provider-rs/releases/download/v${version}/bgutil-pot-linux-${arch}";
    inherit (platform) hash;
  };

  nativeBuildInputs = [ autoPatchelfHook ];
  buildInputs = [
    openssl
    stdenv.cc.cc.lib
  ];

  dontUnpack = true;

  installPhase = ''
    install -Dm755 $src $out/bin/bgutil-pot
  '';

  meta = {
    description = "BgUtils PO token provider server for yt-dlp YouTube bot-check bypass";
    homepage = "https://github.com/jim60105/bgutil-ytdlp-pot-provider-rs";
    license = lib.licenses.gpl3Plus;
    mainProgram = "bgutil-pot";
    platforms = [
      "x86_64-linux"
      "aarch64-linux"
    ];
  };
}
