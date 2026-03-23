{
  lib,
  stdenv,
  deno,
  fetchFromGitHub,
  fetchurl,
  ffmpeg,
  file,
  makeWrapper,
  nodejs,
  pnpm,
  python3,
}:
let
  pname = "metube";
  version = "2026.03.14";
  bgutilStatics = (import ../../static.nix).bgutil-pot;

  src = fetchFromGitHub {
    owner = "alexta69";
    repo = "metube";
    rev = version;
    hash = "sha256-HCJEQuvcFRIRQRFDGE9tIHwAUmLkRi6JQ4IWl44mSp4=";
  };

  # yt-dlp plugin from jim60105/bgutil-ytdlp-pot-provider-rs, which is the
  # companion plugin for the bgutil-pot server.  The nixpkgs package
  # bgutil-ytdlp-pot-provider is a different project at an incompatible
  # major version.
  bgutilPlugin = python3.pkgs.buildPythonPackage {
    pname = "bgutil-ytdlp-pot-provider-rs";
    version = bgutilStatics.version;
    format = "other";
    src = fetchurl {
      url = "https://github.com/jim60105/bgutil-ytdlp-pot-provider-rs/releases/download/v${bgutilStatics.version}/bgutil-ytdlp-pot-provider-rs.zip";
      hash = bgutilStatics.plugin.hash;
    };
    dontUnpack = true;
    installPhase = ''
      mkdir -p "$out/${python3.sitePackages}"
      ${python3}/bin/python3 -m zipfile -e "$src" plugin-src/
      cp -r plugin-src/yt_dlp_plugins "$out/${python3.sitePackages}/"
    '';
  };

  # Python runtime with all backend dependencies.
  pythonEnv = python3.withPackages (
    ps: with ps; [
      aiohttp
      bgutilPlugin
      curl-cffi
      mutagen
      python-socketio
      watchfiles
      yt-dlp
    ]
  );

  # Angular frontend built with pnpm.
  frontend = stdenv.mkDerivation (finalAttrs: {
    pname = "${pname}-frontend";
    inherit version src;
    sourceRoot = "${finalAttrs.src.name}/ui";

    pnpmDeps = pnpm.fetchDeps {
      inherit (finalAttrs)
        pname
        version
        src
        sourceRoot
        ;
      fetcherVersion = 2;
      hash = "sha256-4IJQVaC9mBWbKlygyqcqbnefpYyQWz0wb9y0xU++/9k=";
    };

    nativeBuildInputs = [
      nodejs
      pnpm.configHook
    ];

    buildPhase = ''
      runHook preBuild
      pnpm run build
      runHook postBuild
    '';

    # Output is dist/metube/{browser,...} relative to sourceRoot.
    installPhase = ''
      cp -r dist/metube $out
    '';
  });
in
stdenv.mkDerivation {
  inherit pname version src;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    # Install the Python app alongside the pre-built frontend so that
    # BASE_DIR can point at a single root directory.
    mkdir -p $out/share/metube/ui/dist
    cp -r app $out/share/metube/
    cp -r ${frontend} $out/share/metube/ui/dist/metube

    makeWrapper ${pythonEnv}/bin/python3 $out/bin/metube \
      --add-flags "$out/share/metube/app/main.py" \
      --set BASE_DIR "$out/share/metube" \
      --prefix PATH : ${pythonEnv}/bin \
      --prefix PATH : ${
        lib.makeBinPath [
          deno
          ffmpeg
          file
        ]
      }
  '';

  meta = {
    description = "Web GUI for yt-dlp with playlist support";
    homepage = "https://github.com/alexta69/metube";
    license = lib.licenses.agpl3Plus;
    mainProgram = "metube";
    platforms = lib.platforms.linux;
  };
}
