let
  toConstantCase = lib: s:
    let
      isLower = c: lib.strings.match "[a-z]" c != null;
      isUpper = c: lib.strings.match "[A-Z]" c != null;
      isDigit = c: lib.strings.match "[0-9]" c != null;
      s1 = lib.strings.replaceStrings [ "-" ] [ "_" ] s;
      # Insert underscores between lower→upper boundaries: fooBar →
      # foo_Bar.
      s2 = builtins.replaceStrings
        # regex not available in nix, so use string iteration trick:
        # Instead, split on capital letters.
        # We'll implement below via manual scan.
        [] [] s1;
      # A helper to split camel/PascalCase into words
      splitCamel = str:
        let
          chars = lib.stringToCharacters str;
          step = { acc, word, }:
            c:
            if isLower c || isDigit c then
              { acc = acc; word = word + c; }
            else
              # Uppercase boundary.
              { acc = acc ++ [word]; word = c; };
          final = builtins.foldl' step { acc = []; word = ""; } chars;
        in final.acc ++ [ final.word ];
      words = builtins.filter (w: w != "") (splitCamel s2);
    in
      lib.strings.toUpper (lib.concatStringsSep "_" words);


in {

  inherit toConstantCase;

}
