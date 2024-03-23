{ pkgs }:
{ url
, name ? null
, sha256
# }: nixpkgs.fetchurl {
}: builtins.fetchurl {
  inherit name url sha256;
}
