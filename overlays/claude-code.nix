################################################################################
# Claude Code 2.1.x ships as a native compiled binary instead of a Node.js
# bundle.  Platform-specific npm packages (e.g. @anthropic-ai/claude-code-
# darwin-arm64) each contain a single Mach-O or ELF executable.
#
# This overlay fetches the appropriate platform binary and installs it
# directly — no Node.js runtime, no npm install, no buildNpmPackage.
################################################################################
{ flake-inputs, system, ... }:
final: prev:
let
  statics = (import ../static.nix).claude-code;
  inherit (statics) version;

  # Map Nix system identifiers to the npm package platform suffixes.
  platformMap = {
    "aarch64-darwin" = "darwin-arm64";
    "x86_64-darwin" = "darwin-x64";
    "x86_64-linux" = "linux-x64";
    "aarch64-linux" = "linux-arm64";
  };

  suffix =
    platformMap.${system} or (throw "claude-code: unsupported system ${system}");
  hash = statics.${system}.hash;

  src = final.fetchzip {
    url = "https://registry.npmjs.org/@anthropic-ai/claude-code-${suffix}/-/claude-code-${suffix}-${version}.tgz";
    inherit hash;
  };
in
{
  claude-code = final.stdenv.mkDerivation {
    pname = "claude-code";
    inherit version src;

    nativeBuildInputs = [ final.makeWrapper ];

    # The binary is self-contained; do not strip or patch it.
    dontBuild = true;
    dontStrip = true;
    dontPatchELF = true;

    installPhase = ''
      runHook preInstall
      install -Dm755 claude $out/bin/claude
      runHook postInstall
    '';

    postInstall = ''
      wrapProgram $out/bin/claude \
        --set DISABLE_AUTOUPDATER 1 \
        --unset DEV
    '';

    doInstallCheck = true;
    installCheckPhase = ''
      # Smoke test: the binary should report its version.
      ver="$($out/bin/claude --version 2>/dev/null || true)"
      if ! echo "$ver" | grep -q "${version}"; then
        echo "Version check failed.  Expected ${version}, got: $ver" >&2
        exit 1
      fi
    '';

    meta = with final.lib; {
      description = "Agentic coding tool that lives in your terminal";
      homepage = "https://github.com/anthropics/claude-code";
      license = licenses.unfree;
      mainProgram = "claude";
    };
  };
}
