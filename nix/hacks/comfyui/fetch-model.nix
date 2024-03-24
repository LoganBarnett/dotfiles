{ pkgs }:
{ url
, name ? null
, format ? null
, sha256
# I think builtins.fetchurl _can_ show progress but needs --verbose to do so.
# Need to confirm if this is any better than alternatives (like
# nixpkgs.fetchurl).
}: builtins.fetchurl {
  inherit name url sha256;
} // { inherit format; }
