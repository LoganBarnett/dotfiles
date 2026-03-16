################################################################################
# Provide the uniteai package from: https://github.com/freckletonj/uniteai
#
# Provides an LSP server for an LLM API.
#
# I couldn't get this entirely working, but this might help someone else who
# wants to see it added.  Also I was turned off by ChatGPT's separate
# subscription required for API access.
################################################################################
{
  lib,
  fetchPypi,
  fetchFromGitHub,
  python3,
  python3Packages,
  openai,
  ...
}: let
  python = python3;
  pypkgs = python3Packages;
  pname = "uniteai";
  version = "0.3.0";
  # version = "0.3.0-unstable-2025-01-26";
in python.pkgs.buildPythonApplication {
  inherit pname version;
  pyproject = true;
  build-system = with pypkgs; [ setuptools hatchling ];
  nativeBuildInputs = [];
  # Don't use fetchPypi - we can't control optionals with it.
  src = fetchPypi {
    inherit pname version;
    hash = "sha256-HFH/igxS1Jye0m9jCJ3mNSGu3ecNQtj5lPs9++DdxYQ=";
  };
  # src = fetchFromGitHub {
  #   owner = "freckletonj";
  #   repo = "uniteai";
  #   rev = "2d153741e2fb05497b7e588d174ca1d8a12025b4";
  #   hash = "sha256-4JAi7MRT3e4JVfywcfQyAL/1K0V0mwUhF48RJG9IgYw=";
  # };
  # buildPhase = ''
  #   ${python.interpreter} setup.py build
  # '';
  # preBuild = ''
  #   cd uniteai
  # '';
  preBuild = ''
    ln -s uniteai/.uniteai.yml.example .uniteai.yml.example
  '';
  # Only build the "openai" optional-packages.
  extras = [ "openai" ];
  # TODO: Figure out how to build variants of this package based on what
  # features are to be exposed.
  # propagatedBuildInputs = with pypkgs ; let
  dependencies = with pypkgs; let
  # propagatedBuildInputs = with pypkgs; let
    base = [
      pygls
      thespian
      pyyaml
      requests
      # Strangely a runtime requirement.  Without this, we see a missing
      # pkg_resources import.
      setuptools
      # openai
      # Not declared in the pyproject.toml, but seems to be needed anyways.
      fastapi
      # # Required by the LSP server regardless of optionals.
      # pypdf
      # # Not declared in the pyproject.toml, but seems to be needed anyways.
      # beautifulsoup4
    ];
    local-llm = [
      accelerate
      bitsandbytes
      einops
      fastapi
      scipy
      transformers
      uvicorn
      sentencepiece
      autowq
    ];
    # The pyproject.toml calls this section "openai", but naming it this causes
    # a crash of Nix itself.
    with-openai = [
      openai
    ];
    transcription = [
      pyaudio
      speechrecognition
      openai-whisper
      soundfile
    ];
    speech_to_text = [
      tts
      sounddevice
    ];
    document_chat = [
      gitpython
      nbformat
      youtube-transcript-api
      # Seems to be called `instructor` in nixpkgs.
      # instructorembedding
      instructor
      sentence-transformers
      pypdf
      beautifulsoup4
      pandas
    ];
  in [] ++ base ++ document_chat ++ local-llm ++ with-openai ++ transcription ++ speech_to_text;
  meta = {
    description = "Provides an LSP server for communicating with an LLM API.";
    homepage = "https://github.com/freckletonj/uniteai";
    license = lib.licenses.asl20;
    platforms = lib.platforms.unix;
    maintainers = with lib.maintainers; [ logan-barnett ];
    mainProgram = "uniteai_lsp";
  };
}
