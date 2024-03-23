{ pkgs }:
{ url
, name
, sha256
# }: nixpkgs.fetchurl {
}: builtins.fetchurl {
  inherit name url sha256;
}
