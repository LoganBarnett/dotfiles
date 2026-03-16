################################################################################
# Install the gh CLI for managing repos over GitHub.
################################################################################
{ pkgs, lib, ... }: let
  gh-bbs2gh = pkgs.stdenvNoCC.mkDerivation (let
    version = "1.19";
    # Map Nix platform -> GitHub release suffix like "linux-amd64"
    ghAssetSuffix =
      let
        # e.g. "x86_64-linux", "aarch64-darwin"
        sys   = pkgs.stdenv.hostPlatform.system;
        parts = builtins.match "([^\\-]+)-([^\\-]+)" sys;
        arch  = builtins.elemAt parts 0; # "x86_64" | "aarch64"
        os    = builtins.elemAt parts 1; # "linux"  | "darwin"
        archMap = { x86_64 = "amd64"; aarch64 = "arm64"; };
        osMap   = { linux = "linux";  darwin  = "darwin"; };
        # Right now aarch64 isn't supported and is worked around via reliance on
        # Rosetta: https://github.com/github/gh-gei/issues/302
        a = if os == "darwin"
            then "amd64"
            else (archMap.${arch} or (throw "Unsupported CPU: ${arch}"))
        ;
        o = osMap.${os}     or (throw "Unsupported OS: ${os}");
      in "${o}-${a}";
  in {
    pname = "gh-bbs2gh";
    version = "v${version}";
    src = pkgs.fetchurl {
      url = "https://github.com/github/gh-bbs2gh/releases/download/v${version}/bbs2gh-${ghAssetSuffix}";
      sha256 = "sha256-Nt4X3IeqG4SXC6yyrVwDKIgm/lrndh812QcWkZYqbEE=";
    };
    # It's just a raw binary.
    dontUnpack = true;
    installPhase = ''
      install -Dm755 "$src" "$out/bin/gh-bbs2gh"
    '';
  });
in {
  programs.gh = {
    enable = true;
    settings = {
      # Just for reference.
      # aliases = {
      #   co = "pr checkout";
      #   pv = "pr view";
      # };
      git_protocol = "ssh";
      # gitCredentialHelper.hosts = [
      #   (lib.strings.concatStrings [
      #     "https://"
      #     "h"
      #     "m"
      #     "h"
      #     ".ghe.com"
      #   ])
      # ];
    };
    extensions = [
      # For migrating from Stash / Bitbucket Server / BitBucket DataCenter.
      gh-bbs2gh
      pkgs.gh-s
    ];
  };
}
