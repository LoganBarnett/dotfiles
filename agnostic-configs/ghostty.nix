################################################################################
# Install the Ghostty terminal emulator as a system package so macOS launchers
# like Spotlight and Alfred can find the app bundle in /Applications/Nix Apps/.
#
# ghostty-bin is used on macOS because the ghostty package builds all of X
# Windows as a dependency.  ghostty-bin fetches the official pre-built macOS
# binary instead.
#
# See also ../home-configs/ghostty.nix for terminal settings and shell
# integration.
################################################################################
{ pkgs, ... }:
{
  environment.systemPackages = [
    (if pkgs.stdenv.isDarwin then pkgs.ghostty-bin else pkgs.ghostty)
  ];
}
