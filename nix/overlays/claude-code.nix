################################################################################
# Claude Code updates frequently and the package in nixpkgs can fall behind.
# This overlay allows us to override the version without updating all of
# nixpkgs.
################################################################################
{ flake-inputs, system, ... }: final: prev: let
  statics = (import ../static.nix).claude-code;
  inherit (statics) version hash npmDepsHash;
in {
  claude-code = prev.claude-code.overrideAttrs (old: {
    inherit version;
    src = final.fetchzip {
      url = "https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-${version}.tgz";
      inherit hash;
    };
    inherit npmDepsHash;
  });
}
