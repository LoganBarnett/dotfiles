{ lib, pkgs }:
{ url
# , name ? null
, format ? null
, sha256
, bearer ? null
}: {
  inherit format;
  # I think builtins.fetchurl _can_ show progress but needs --verbose to do so.
  # Need to confirm if this is any better than alternatives (like
  # nixpkgs.fetchurl).
  # path = builtins.fetchurl ({
  path = (pkgs.fetchurl ({
    name = lib.replaceStrings ["?" "&" ":" "/"] ["__questionmark__" "__ampersand__" "__colon__" "__slash__"] url;
    inherit url sha256;
  }
    // (lib.optionalAttrs (bearer != null) {
      # The closest thing to documentation for curlOptsList that I've found:
      # https://github.com/NixOS/nixpkgs/issues/41820#issuecomment-396120262
      curlOptsList = [
        "--header" "Authorization: Bearer ${bearer}"
      ];
    })
  ))
    # Normally a derivation is coerced to a string when interpolated where it is
    # used.  That is not observed here (for reasons not well understood).
    # Derivations have an outPath which contains what we actually want, so just
    # grab it.  Note that this only happens with pkgs.fetchurl, and
    # builtings.fetchurl does not suffer this issue.
    .outPath;
}
