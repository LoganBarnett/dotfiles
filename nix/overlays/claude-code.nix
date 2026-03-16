################################################################################
# Claude Code updates frequently and the package in nixpkgs can fall behind.
# This overlay allows us to override the version without updating all of
# nixpkgs.
#
# The npm tarball does not include package-lock.json, so nixpkgs commits one
# alongside the package definition and copies it in via postPatch before
# fetchNpmDeps runs.  We do the same by keeping a versioned lock file here.
#
# buildNpmPackage uses lib.extendMkDerivation, but overrideAttrs does not
# cause extendDrvArgs to recompute npmDeps with the new src — it is computed
# from the original fpargs.  We must therefore provide npmDeps explicitly.
# fetchNpmDeps is stdenvNoCC.mkDerivation and supports postPatch natively, so
# we use it to copy the generated lock file before prefetch-npm-deps runs.
################################################################################
{ flake-inputs, system, ... }: final: prev: let
  statics = (import ../static.nix).claude-code;
  inherit (statics) version hash npmDepsHash;
  src = final.fetchzip {
    url = "https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-${version}.tgz";
    inherit hash;
  };
  lockFile = ./claude-code-package-lock.json;
in {
  claude-code = prev.claude-code.overrideAttrs (old: {
    inherit version src npmDepsHash;
    postPatch = ''
      cp ${lockFile} package-lock.json
    '';
    npmDeps = final.fetchNpmDeps {
      inherit src;
      postPatch = ''
        cp ${lockFile} package-lock.json
      '';
      name = "claude-code-${version}-npm-deps";
      hash = npmDepsHash;
    };
    # All of claude-code's declared dependencies are optional platform
    # binaries (sharp).  npm ci --ignore-scripts leaves node_modules
    # uncreated, which causes npmInstallHook to crash.  An empty directory
    # satisfies the hook's copy step without shipping unusable native code.
    preInstall = ''
      mkdir -p node_modules
    '';
  });
}
