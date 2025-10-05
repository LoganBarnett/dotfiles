################################################################################
# Invariably, one must generate some YAML instead of just serializing to it.
# This is typically due to the YAML containing secrets (gross) or the YAML makes
# use of non-inert features that allow self referencing and all kinds of stuff
# you probably shouldn't see in a data structure format (beyond gross).  These
# non-inert features do not round-trip through YAML serialization and
# deserialization, and that include's Nix's YAML machinery.
#
# And, because YAML is a significant whitespace language, you have to know, very
# precisely, where in the document you are templating out to.  So you need
# helpers like below to help you achieve that.  But you'll probably get
# off-by-one indentation issues because you didn't flush your list entries with
# the double-single-quote string.  So look out for that.
#
# This kind of proves why, proven mathematically, that significant whitespace is
# a pox upon our existence, but now we're getting into blog post territory and
# I'll stop here.
################################################################################
{ lib, ... }: let
  inherit (lib.strings) concatStringsSep splitString;
in {
  ##
  # Pad all non-empty lines by `spaces`.
  #
  # Warning: When using Nix double-single-quote strings, the indentation of the
  # string conents is stripped until it reaches the first non-whitespace column,
  # or the column where the string literal ends (a `''` set).  When using
  # indentLines, be aware of this to prevent the "why is my first line not
  # indented correctly?" problem.
  #
  # :: Number -> String -> String
  indentLines = spaces: s:
    let
      prefix = concatStringsSep "" (builtins.genList (_: " ") spaces);
      lines  = splitString "\n" s;
      ind    = builtins.map (l: if l == "" then l else prefix + l) lines;
    in concatStringsSep "\n" ind;
}
