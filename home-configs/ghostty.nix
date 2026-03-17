################################################################################
# Configure Ghostty terminal emulator settings and shell integration.
#
# ghostty-bin is used on macOS because the ghostty package builds all of X
# Windows as a dependency.  ghostty-bin fetches the official pre-built macOS
# binary instead.  mac-app-util creates a trampoline in
# ~/Applications/Home Manager Trampolines/ so Alfred and Spotlight can find it.
################################################################################
{
  config,
  lib,
  pkgs,
  ...
}:
{
  programs.ghostty = {
    enable = true;
    package = if pkgs.stdenv.isDarwin then pkgs.ghostty-bin else pkgs.ghostty;
    enableZshIntegration = true;
    settings = {
      # Ghostty's fixterms encoding sends Ctrl+[ as ^[[91;5u instead of the
      # traditional \x1b, which zsh vi-mode doesn't recognize.  This remaps it
      # back to plain ESC so vi-cmd-mode triggers correctly.
      keybind = "ctrl+bracket_left=text:\\x1b";
      background = "#0d0d0d";
      foreground = "#f2f2f2";
      maximize = true;
      # This keeps the title bar from being totally black so I can't tell where
      # the title bar ends and the terminal background begins.
      macos-titlebar-style = "native";
      # display-p3 makes colors match Terminal.app's vibrancy.  Terminal.app
      # passes palette hex values through to the display natively rather than
      # converting from sRGB, so using display-p3 here matches that behavior.
      window-colorspace = "display-p3";
      font-family = "Source Code Pro";
      font-size = 14;
      # Ghostty's GPU renderer produces thinner strokes than Terminal.app's
      # CoreText renderer.  On dark backgrounds this makes colored text appear
      # faded and harder to tell apart.  font-thicken compensates (macOS only).
      font-thicken = true;
      # Mirror the Terminal.app "Pro" profile ANSI palette so prompt colors
      # match between the two terminals.  Indices 4 and 12 (blue and bright
      # blue) come from the exported Pro.terminal profile; the remaining 14
      # are Terminal.app's compiled-in defaults, which are not stored in any
      # user-accessible plist.
      palette = [
        "0=#000000"
        "1=#990000"
        "2=#00a600"
        "3=#999900"
        "4=#005684"
        "5=#b300b3"
        "6=#00a6b3"
        "7=#bfbfbf"
        "8=#666666"
        "9=#e60000"
        "10=#00d900"
        "11=#e6e600"
        "12=#0086ff"
        "13=#e600e6"
        "14=#00e6e6"
        "15=#e6e6e6"
      ];
    };
  };
}
