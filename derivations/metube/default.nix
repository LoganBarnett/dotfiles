{
  lib,
  stdenv,
  fetchFromGitHub,
  file,
  makeWrapper,
  nodejs,
  pnpm,
  python3,
}:
let
  pname = "metube";
  version = "2026.03.14";
  src = fetchFromGitHub {
    owner = "alexta69";
    repo = "metube";
    rev = version;
    hash = "sha256-HCJEQuvcFRIRQRFDGE9tIHwAUmLkRi6JQ4IWl44mSp4=";
  };

  # Python runtime with all backend dependencies.
  pythonEnv = python3.withPackages (
    ps: with ps; [
      aiohttp
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
      --prefix PATH : ${lib.makeBinPath [ file ]}
  '';

  meta = {
    description = "Web GUI for yt-dlp with playlist support";
    homepage = "https://github.com/alexta69/metube";
    license = lib.licenses.agpl3Plus;
    mainProgram = "metube";
    platforms = lib.platforms.linux;
  };
}
