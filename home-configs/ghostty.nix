################################################################################
# Configure Ghostty terminal emulator.
#
# ghostty-bin is used on macOS because the ghostty package builds all of X
# Windows as a dependency.  ghostty-bin fetches the official pre-built macOS
# binary instead.
################################################################################
{ lib, pkgs, ... }:
{
  programs.ghostty = {
    enable = true;
    package = if pkgs.stdenv.isDarwin then pkgs.ghostty-bin else pkgs.ghostty;
  };
}
