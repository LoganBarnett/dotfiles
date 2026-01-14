{ lib
, fetchFromGitHub
, stdenv
, tree-sitter
}:

tree-sitter.buildGrammar {
  language = "jq";
  version = "unstable-2025-02-26";

  src = fetchFromGitHub {
    owner = "nverno";
    repo = "tree-sitter-jq";
    rev = "1e139eba1fd3a9c34a36f0f0f47ed8b73c9b4636";
    hash = "sha256-1lKg/mQdjNMdiKvFBf4aAkGBBNKCnJ+OOqRfZJ8ly4M=";
  };

  meta = {
    description = "Tree-sitter grammar for jq";
    homepage = "https://github.com/nverno/tree-sitter-jq";
    license = lib.licenses.gpl3Only;
    platforms = lib.platforms.unix;
  };
}
