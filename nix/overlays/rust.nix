# Per
# https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/rust.section.md#using-community-maintained-rust-toolchains
# Fenix is a community maintained package for Rust "toolchains" and the like.
# This is used to give us rust-docs, and possibly other things. We want this
# stuff installed in advance because we want rust-docs to just be available and
# not need to remember installing it when the day comes to use it (which will
# invariably be a time when Internet access is non-existant.
#
# Use `rustup doc --book` to view the docs. There are possibly other useful
# invocations as well.
self: super: {
  fenix = (super.fenix or {}) //
    (super.callPackage
      (super.fetchFromGitHub {
        owner = "nix-community";
        repo = "fenix";
        # Commit from 2023-07-18.
        # Unsure why hash doesn't need updating from example.
        rev = "cb4ed6a92123dd8e3befdf0fbed27c9ebb8e7176";
        hash = "sha256-AsOim1A8KKtMWIxG+lXh5Q4P2bhOZjoUhFWJ1EuZNNk=";
      })
      { }
    );
}
