################################################################################
# Block rustup on Nix hosts.
#
# Rustup has no place on a Nix-managed machine.  Each project that needs Rust
# exposes a devshell (nix develop) which provides the correct toolchain
# version.  Pointing ~/.rustup at /dev/null prevents accidental installation
# and makes the intent explicit.
################################################################################
{ config, ... }:
{
  home.file.".rustup".source = config.lib.file.mkOutOfStoreSymlink "/dev/null";
}
