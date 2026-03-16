################################################################################
# When people say "Copilot", this is what they mean.  There is an AWS Copilot (a
# competitor fork?).
#
# My current Nixpkgs version is quite a bit behind, given how fast things are
# moving for github-copilot-cli.  They even moved repos at some point, which my
# old version (the latest Nixpkgs stable) points at the old repo.
#
# Using `overrideAttrs` doesn't work per
# https://wiki.nixos.org/wiki/Node.js#Override_NodeJS_package but its counter
# instructions also don't work well, so just provide the entire derivation.  You
# will also want to run this to update the copilot-package-lock.json when an
# update is needed:
# npm-generate-package-lock-json https://registry.npmjs.org/@github/copilot/-/copilot-0.0.342.tgz copilot
#
# And then copy the file as needed to the derivations directory - make sure it
# is named `copilot-package-lock.json`, which the program should do for you.
# You can find the script declared here:
# ../scripts/npm-generate-package-lock-json.sh
################################################################################
{
  lib,
  buildNpmPackage,
  fetchzip,
  nix-update-script,
}: let
  version = "0.0.342";
in buildNpmPackage (finalAttrs: {
  pname = "github-copilot-cli";
  inherit version;

  src = fetchzip {
    url = "https://registry.npmjs.org/@github/copilot/-/copilot-${version}.tgz";
      hash = "sha256-NY/ledtHdF/9ha+ZjstFRsc3zm5Vy5I8sl5wGt2+A64=";
  };

  npmDepsHash = "sha256-TopIIfO6W3x+gtfGZRSq4BHd+jdI3+8/LaW+eOnMAkY=";

  postPatch = ''
    cp ${./copilot-package-lock.json} package-lock.json
  '';

  dontNpmBuild = true;

  passthru.updateScript = nix-update-script { extraArgs = [ "--generate-lockfile" ]; };

  meta = {
    description = "GitHub Copilot CLI brings the power of Copilot coding agent directly to your terminal";
    homepage = "https://github.com/github/copilot-cli";
    changelog = "https://github.com/github/copilot-cli/releases/tag/v${finalAttrs.version}";
    downloadPage = "https://www.npmjs.com/package/@github/copilot";
    license = lib.licenses.unfree;
    maintainers = with lib.maintainers; [
      dbreyfogle
    ];
    mainProgram = "copilot";
  };
})
