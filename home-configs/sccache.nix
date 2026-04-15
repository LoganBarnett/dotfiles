################################################################################
# sccache — shared Rust compilation cache and centralised build directory.
#
# Sits transparently in front of rustc via RUSTC_WRAPPER.  Artifacts are
# shared across all projects that use the same crate version, so dependencies
# like tokio and axum are compiled once rather than once per project.
#
# CARGO_TARGET_DIR routes all cargo build output to a single directory
# (~/.cache/cargo-target) instead of per-project target/ directories.  This
# avoids duplicating built dependencies across 20+ projects and prevents
# disk exhaustion.
#
# The sccache cache lives at ~/.cache/sccache and is capped at 20 GB; LRU
# entries are evicted automatically when the limit is reached.
#
# To inspect cache statistics: sccache --show-stats
# To clear the cache:          sccache --stop-server && rm -rf ~/.cache/sccache
################################################################################
{ pkgs, config, ... }:
{
  home.packages = [ pkgs.sccache ];

  home.sessionVariables = {
    CARGO_TARGET_DIR = "${config.home.homeDirectory}/.cache/cargo-target";
    RUSTC_WRAPPER = "${pkgs.sccache}/bin/sccache";
    SCCACHE_DIR = "${config.home.homeDirectory}/.cache/sccache";
    SCCACHE_CACHE_SIZE = "20G";
  };
}
