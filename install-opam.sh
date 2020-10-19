#! /usr/bin/env bash
set -e

# Contributing to Flow was the primary motivator here. For now it's disabled.

# I'm guessing that -y will accept the shell integration question. Perhaps it
# should be -n and we just check in the configured settings.
opam init -y --comp 4.03.0
eval $(opam config env)
echo "getting other ocaml dev tools"
opam update

# js_of_ocaml won't build and I haven't been able to fix it.
opam install -y ocamlfind sedlex # js_of_ocaml
