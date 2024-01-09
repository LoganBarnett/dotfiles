##
# Manage my various macOS settings here.  These would be typically configured
# via preference panes and the like, but Nix can actually handle quite a bit of
# this.
#
# Per https://github.com/LnL7/nix-darwin/issues/658 some of these changes
# require restarting the process (ie `killall Dock`), logging out, or
# restarting.
#
# /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u

{ pkgs, ... }:
{
  # From the nix-darwin readme (https://github.com/LnL7/nix-darwin):
  # Auto upgrade nix package and the daemon service.
  nix.package = pkgs.nix;
  security.pam.enableSudoTouchIdAuth = true;
  # Without this, nothing works.
  services.nix-daemon.enable = true;
  system = {
    defaults = {
      NSGlobalDomain = {
        # Dark theme. null is the only other valid value.
        AppleInterfaceStyle = "Dark";
        # Replace press-and-hold with key repeat.
        ApplePressAndHoldEnabled = false;
        # Expand save panel by default.
        NSNavPanelExpandedStateForSaveMode = true;
        # Disable autocorrect capitalization.
        NSAutomaticCapitalizationEnabled = false;
        # Disable autocorrect smart dashes.
        NSAutomaticDashSubstitutionEnabled = false;
        # Disable autocorrect adding periods.
        NSAutomaticPeriodSubstitutionEnabled = false;
        # Disable autocorrect smart quotation marks.
        NSAutomaticQuoteSubstitutionEnabled = false;
        # Disable autocorrect spellcheck.
        NSAutomaticSpellingCorrectionEnabled = false;
        # Expand print panel by default.
        PMPrintingExpandedStateForPrint = true;
        # Use a long key repeat, to make it agonizing use pedestrian key
        # bindings.
        KeyRepeat = 100;
      };
      # Refresh with `killall Dock`.
      dock = {
        # Automatically show and hide the dock
        autohide = true;
        # Add translucency in dock for hidden applications
        showhidden = true;
        # Enable spring loading on all dock items
        enable-spring-load-actions-on-all-items = true;
        # Highlight hover effect in dock stack grid view
        mouse-over-hilite-stack = true;
        mineffect = "genie";
        orientation = "bottom";
        # Specifically, show _Applications_ in the Recent Items Apple menu.
        show-recents = false;
        tilesize = 44;
      };
    };
    keyboard = {
      enableKeyMapping = true; # Required to make remapping work.
      remapCapsLockToControl = true;
    };
  };
}
