{ lib }:
{ url
# , name ? null
, format ? null
, sha256
}: {
  inherit format;
  # I think builtins.fetchurl _can_ show progress but needs --verbose to do so.
  # Need to confirm if this is any better than alternatives (like
  # nixpkgs.fetchurl).
  path = builtins.fetchurl {
    name = lib.replaceStrings ["?" "&" ":" "/"] ["__questionmark__" "__ampersand__" "__colon__" "__slash__"] url;
    inherit url sha256;
  };
}
