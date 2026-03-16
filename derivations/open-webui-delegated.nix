################################################################################
# Build a version of open-webui that doesn't require compiling all of the CUDA
# bits.  It is intended strictly for delegating.  Ultimately copied and modified
# from:
# https://github.com/NixOS/nixpkgs/blob/nixos-25.05/pkgs/by-name/op/open-webui/package.nix
################################################################################
{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  python312,
  nixosTests,
  fetchurl,
  runCommand,
}:
let
  python = python312.override {
    packageOverrides = (final: prev: let
      buildDummyPythonPackage = { pname, ... }: {
        inherit pname;
        version = "0.0.0-dummy";
        src = runCommand "empty-faiss-src" { } ''
          mkdir -p $out
        '';
        format = "other";
        buildPhase = "true";
        installPhase = "mkdir -p $out";
        meta = {
          description = "Stub faiss to prevent unwanted builds.";
          license = prev.lib.licenses.mit;
        };
      };
    in {
      faiss = final.buildDummyPythonPackage {
        pname = "faiss";
      };
      # faiss = prev.faiss.overridePythonAttrs (_: {
      #   disabled = true;
      # });
    });
  };
  pname = "open-webui";
  version = "0.6.9";

  src = fetchFromGitHub {
    owner = "open-webui";
    repo = "open-webui";
    tag = "v${version}";
    hash = "sha256-Eib5UpPPQHXHOBVWrsNH1eEJrF8Vx9XshGYUnnAehpM=";
  };
  pyodideVersion = "0.27.3";
  # the backend for run-on-client-browser python execution
  # must match lock file in open-webui
  # TODO: should we automate this?
  # TODO: with JQ? "jq -r '.packages["node_modules/pyodide"].version' package-lock.json"
  pyodide = fetchurl {
    hash = "sha256-SeK3RKqqxxLLf9DN5xXuPw6ZPblE6OX9VRXMzdrmTV4=";
    url = "https://github.com/pyodide/pyodide/releases/download/${pyodideVersion}/pyodide-${pyodideVersion}.tar.bz2";
  };

  frontend = buildNpmPackage {
    inherit pname version src pyodide;

    npmDepsHash = "sha256-Vcc8ExET53EVtNUhb4JoxYIUWoQ++rVTpxUPgcZ+GNI=";

    # Disabling `pyodide:fetch` as it downloads packages during `buildPhase`
    # Until this is solved, running python packages from the browser will not
    # work.
    postPatch = ''
      substituteInPlace package.json \
        --replace-fail "npm run pyodide:fetch && vite build" "vite build"
    '';
    # Disallow cypress from downloading binaries in sandbox.
    env.CYPRESS_INSTALL_BINARY = "0";
    env.ONNXRUNTIME_NODE_INSTALL_CUDA = "skip";
    env.NODE_OPTIONS = "--max-old-space-size=8192";

    preBuild = ''
      tar xf ${pyodide} -C static/
    '';

    installPhase = ''
      runHook preInstall

      mkdir -p $out/share
      cp -a build $out/share/open-webui

      runHook postInstall
    '';
  };
in
python.pkgs.buildPythonApplication rec {
  inherit pname version src;
  pyproject = true;

  build-system = [ python.pkgs.hatchling ];

  buildPhase = ''
    export TORCH_CUDA_ARCH_LIST=""
    export USE_CUDA=0
    export CMAKE_ARGS="-DUSE_CUDA=OFF"
    export CUDA_VISIBLE_DEVICES=""
    ${python.interpreter} setup.py build
  '';

  # Not force-including the frontend build directory as frontend is managed by
  # the `frontend` derivation above.
  postPatch = ''
    substituteInPlace pyproject.toml \
      --replace-fail ', build = "open_webui/frontend"' ""
  '';

  env.HATCH_BUILD_NO_HOOKS = true;

  pythonRelaxDeps = true;

  pythonRemoveDeps = [
    "docker"
    "pytest"
    "pytest-docker"
  ];

  dependencies =
    with python.pkgs;
    [
      accelerate
      aiocache
      aiofiles
      aiohttp
      alembic
      anthropic
      apscheduler
      argon2-cffi
      asgiref
      async-timeout
      authlib
      azure-ai-documentintelligence
      azure-identity
      azure-storage-blob
      bcrypt
      beautifulsoup4
      black
      boto3
      chromadb
      colbert-ai
      docx2txt
      duckduckgo-search
      einops
      elasticsearch
      extract-msg
      fake-useragent
      fastapi
      # faster-whisper
      firecrawl-py
      fpdf2
      ftfy
      gcp-storage-emulator
      google-api-python-client
      google-auth-httplib2
      google-auth-oauthlib
      google-cloud-storage
      google-generativeai
      googleapis-common-protos
      iso-639
      langchain
      langchain-community
      langdetect
      langfuse
      ldap3
      loguru
      markdown
      moto
      nltk
      onnxruntime
      openai
      # opencv-python-headless
      openpyxl
      opensearch-py
      opentelemetry-api
      opentelemetry-sdk
      opentelemetry-exporter-otlp
      opentelemetry-instrumentation
      opentelemetry-instrumentation-fastapi
      opentelemetry-instrumentation-sqlalchemy
      opentelemetry-instrumentation-redis
      opentelemetry-instrumentation-requests
      opentelemetry-instrumentation-logging
      opentelemetry-instrumentation-httpx
      opentelemetry-instrumentation-aiohttp-client
      pandas
      passlib
      peewee
      peewee-migrate
      pgvector
      pillow
      pinecone-client
      playwright
      psutil
      psycopg2-binary
      pydub
      pyjwt
      pymdown-extensions
      pymilvus
      pymongo
      pymysql
      pypandoc
      pypdf
      python-dotenv
      python-jose
      python-multipart
      python-pptx
      python-socketio
      pytube
      pyxlsb
      qdrant-client
      rank-bm25
      rapidocr-onnxruntime
      redis
      requests
      restrictedpython
      # sentence-transformers
      # sentencepiece
      soundfile
      tencentcloud-sdk-python
      tiktoken
      # transformers
      unstructured
      uvicorn
      validators
      xlrd
      youtube-transcript-api
    ]
    ++ moto.optional-dependencies.s3;

  pythonImportsCheck = [ "open_webui" ];

  makeWrapperArgs = [ "--set FRONTEND_BUILD_DIR ${frontend}/share/open-webui" ];

  passthru = {
    tests = {
      inherit (nixosTests) open-webui;
    };
    updateScript = ./update.sh;
    inherit frontend;
  };

  meta = {
    changelog = "https://github.com/open-webui/open-webui/blob/${src.tag}/CHANGELOG.md";
    description = "Comprehensive suite for LLMs with a user-friendly WebUI";
    homepage = "https://github.com/open-webui/open-webui";
    license = lib.licenses.mit;
    mainProgram = "open-webui";
    maintainers = with lib.maintainers; [
      drupol
      shivaraj-bh
    ];
  };
}
