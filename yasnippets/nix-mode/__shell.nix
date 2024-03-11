# -*- mode: snippet; require-file-newline: nil -*-
# name: nix-shell
# key: __shell.nix
# group: yatemplate
# condition: "shell.nix"
# Maybe this could be multi-purpose or something. I'll need to read up on how
# yasnippet can generate different snippets based on different file names.
# --
{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  nativeBuildInputs = [
    # Add pkgs here. Example:
    # pkgs.ruby
  ];
}