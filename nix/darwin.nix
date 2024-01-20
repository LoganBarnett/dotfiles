##
# Manage my various macOS settings here.  These would be typically configured
# via preference panes and the like, but Nix can actually handle quite a bit of
# this.
#
# See https://macos-defaults.com for where you can get a list of these settings.
#
# Per https://github.com/LnL7/nix-darwin/issues/658 some of these changes
# require restarting the process (ie `killall Dock`), logging out, or
# restarting.
#
# /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
#
# Many of these settings are the defaults, but this keeps the settings
# consistent between versions if the defaults should ever change.
#
# Much of this is shamelessly lifted from:
# https://github.com/nmasur/dotfiles/blob/master/modules/darwin/system.nix

{ pkgs, ... }:
# let
#   zsh = import ./zsh.nix { pkgs = pkgs; };
# in
{
  # Global packages that can't be bound to a specific user, such as shells.
  environment = {
    loginShell = pkgs.zsh;
    shells = [ pkgs.zsh ];
    systemPackages = [
      pkgs.zsh
    ];
  };
  # From the nix-darwin readme (https://github.com/LnL7/nix-darwin):
  # Auto upgrade nix package and the daemon service.
  nix.package = pkgs.nix;
  security.pam.enableSudoTouchIdAuth = true;
  # Without this, nothing works.
  services.nix-daemon.enable = true;
  system = {
    # Settings that don't have an option in nix-darwin
    activationScripts.postActivation.text = ''
      echo "Set disk image verification..."
      # I don't know what I want these set to, or what the defaults are, so
      # skipping for now.
      # defaults write com.apple.frameworks.diskimages skip-verify -bool true
      # defaults write com.apple.frameworks.diskimages skip-verify-locked -bool true
      # defaults write com.apple.frameworks.diskimages skip-verify-remote -bool true
      echo "Avoid creating .DS_Store files on network volumes..."
      defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
      echo "Set the warning before emptying the Trash..."
      defaults write com.apple.finder WarnOnEmptyTrash -bool true
      echo "Require password immediately after sleep or screen saver begins..."
      defaults write com.apple.screensaver askForPassword -int 1
      defaults write com.apple.screensaver askForPasswordDelay -int 0
      echo "Allow apps from anywhere..."
      SPCTL=$(spctl --status)
      if ! [ "$SPCTL" = "assessments disabled" ]; then
        sudo spctl --master-disable
      fi
'';
    # User-level settings
    activationScripts.postUserActivation.text = ''
      echo "Show the ~/Library folder..."
      chflags nohidden ~/Library
      echo "Set dock magnification..."
      defaults write com.apple.dock magnification -bool false
      echo "Set dock magnification size..."
      defaults write com.apple.dock largesize -int 48
      echo "Define dock icon function..."
      __dock_item() {
        printf \
           "%s%s%s%s%s" \
           "<dict><key>tile-data</key><dict><key>file-data</key><dict>" \
           "<key>_CFURLString</key><string>" \
           "$1" \
           "</string><key>_CFURLStringType</key><integer>0</integer>" \
           "</dict></dict></dict>"
      }
      echo "Choose and order dock icons"
      defaults write com.apple.dock persistent-apps -array \
          "$(__dock_item /System/Applications/Utilities/Terminal.app)" \
          "$(__dock_item /System/Applications/System\ Settings.app)"

      # Example of writing using something from Nix itself:
      # "$(__dock_item \$\{pkgs.slack}/Applications/Slack.app)"
'';
    defaults = {
      NSGlobalDomain = {
        # Disable left/right swipe to navigate backwards/forwards.
        AppleEnableSwipeNavigateWithScrolls = false;
        # Whether to use 24-hour or 12-hour time. The default is based on region
        # settings.
        AppleICUForce24HourTime = true;
        # Whether to automatically switch between light and dark mode. The
        # default is false.
        AppleInterfaceStyleSwitchesAutomatically = false;
        # Set to ‘Dark’ to enable dark mode, or leave unset for normal mode.
        AppleInterfaceStyle = "Dark";
        # Configures the keyboard control behavior. Mode 3 enables full keyboard
        # control.  This makes it possible to tab to any UI element.
        AppleKeyboardUIMode = 3;
        # Whether to use centimeters (metric) or inches (US, UK) as the
        # measurement unit.  The default is based on region settings.
        # AppleMeasurementUnits = "Centimeters";
        # Whether to use the metric system. The default is based on region
        # settings.
        AppleMetricUnits = 1;
        # Whether to enable the press-and-hold feature.  The default is true.
        # Replace press-and-hold with key repeat.
        ApplePressAndHoldEnabled = false;
        # Jump to the spot that’s clicked on the scroll bar.  The default is
        # false.
        AppleScrollerPagingBehavior = true;
        # Whether to show all file extensions in Finder.  The default is false.
        AppleShowAllExtensions = true;
        # Whether to always show hidden files.  The default is false.
        AppleShowAllFiles = true;
        # When to show the scrollbars.  Options are ‘WhenScrolling’, ‘Automatic’
        # and ‘Always’.
        # AppleShowScrollBars = "Automatic";
        # Whether to use Celsius or Fahrenheit.  The default is based on region
        # settings.
        AppleTemperatureUnit = "Celsius";
        # Sets the window tabbing when opening a new document: ‘manual’,
        # ‘always’, or ‘fullscreen’.  The default is ‘fullscreen’.
        AppleWindowTabbingMode = "fullscreen";
        # If you press and hold certain keyboard keys when in a text area, the
        # key’s character begins to repeat.  For example, the Delete key
        # continues to remove text for as long as you hold it down.
        # This sets how long you must hold down the key before it starts
        # repeating.  See also KeyRepeat.
        InitialKeyRepeat = 500;
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
        # Whether to animate opening and closing of windows and popovers.  The
        # default is true.
        NSAutomaticWindowAnimationsEnabled = true;
        # Whether to disable the automatic termination of inactive apps.
        NSDisableAutomaticTermination = false;
        # Whether to save new documents to iCloud by default.  The default is
        # true.
        NSDocumentSaveNewDocumentsToCloud = false;
        # Expand save panel by default.
        NSNavPanelExpandedStateForSaveMode = true;
        # Expand save panel by default.  Really?
        NSNavPanelExpandedStateForSaveMode2 = true;
        # Whether to enable smooth scrolling.  The default is true.
        NSScrollAnimationEnabled = true;
        # Sets the size of the finder sidebar icons: 1 (small), 2 (medium) or 3
        # (large).  The default is 3.
        # NSTableViewDefaultSizeMode = 3;
        # Whether to display ASCII control characters using caret notation in
        # standard text views.  The default is false.  What they would display
        # when false is not apparent.
        NSTextShowsControlCharacters = true;
        # Whether to enable the focus ring animation.  The default is true.
        NSUseAnimatedFocusRing = true;
        # Sets the speed speed of window resizing.  The default is 0.2.
        NSWindowResizeTime = 0.2;
        # Expand print panel by default.
        PMPrintingExpandedStateForPrint = true;
        # Expand print panel by default.  Really?
        PMPrintingExpandedStateForPrint2 = true;
        # Whether to autohide the menu bar.
        _HIHideMenuBar = false;
        # Use F1, F2, etc. keys as standard function keys.
        "com.apple.keyboard.fnState" = false;
        # Configures the trackpad tap behavior.  Mode 1 enables tap to click.
        "com.apple.mouse.tapBehavior" = 1;
        # Make a feedback sound when the system volume changed.  This setting
        # accepts the integers 0 or 1.
        "com.apple.sound.beep.feedback" = 1;
        # Sets the beep/alert volume level from 0.000 (muted) to 1.000 (100%
        # volume).
        # 75% = 0.7788008
        # 50% = 0.6065307
        # 25% = 0.4723665
        "com.apple.sound.beep.volume" = 1;
        # Set the spring loading delay for directories.
        "com.apple.springing.delay" = 1.0;
        # Whether to enable spring loading (expose) for directories.
        "com.apple.springing.enabled" = true;
        # Whether to enable “Natural” scrolling direction.
        "com.apple.swipescrolldirection" = true;
        # Whether to enable trackpad secondary click.
        "com.apple.trackpad.enableSecondaryClick" = true;
        # Configures the trackpad tracking speed (0.0 to 3.0).
        # "com.apple.trackpad.scaling" = 1.0;
        # Configures the trackpad corner click behavior.  Mode 1 enables right
        # click.  null disables.
        "com.apple.trackpad.trackpadCornerClickBehavior" = null;
        # Use a long key repeat, to make it agonizing use pedestrian key
        # bindings.
        KeyRepeat = 100;
      };
      # Automatically install Mac OS software updates.
      SoftwareUpdate.AutomaticallyInstallMacOSUpdates = false;
      alf = {
        # Allows any downloaded Application that has been signed to accept
        # incoming requests.
        allowdownloadsignedenabled = 0;
        # Allows any signed Application to accept incoming requests.
        allowsignedenabled = 0;
        # Enable the internal firewall to prevent unauthorised applications,
        # programs and services from accepting incoming connections.
        # 0 = Disabled.
        # 1 = Enabled.
        # 2 = Blocks all connections except for essential services.
        globalstate = 0;
        # Enable logging of requests made to the firewall.
        loggingenabled = 0;
        # Drops incoming requests via ICMP such as ping requests.
        stealthenabled = 0;
      };
      # Refresh with `killall Dock`.
      dock = {
        # Whether to display the appswitcher on all displays or only the main
        # one.
        appswitcher-all-displays = false;
        # Automatically show and hide the dock
        autohide = true;
        # Sets the speed of the autohide delay.
        autohide-delay = 0.24;
        # Sets the speed of the animation when hiding/showing the Dock.
        autohide-time-modifier = 1.0;
        # Whether to hide Dashboard as a Space.
        dashboard-in-overlay = false;
        # Sets the speed of the Mission Control animations.
        expose-animation-duration = 1.0;
        # Whether to group windows by application in Mission Control’s Exposé.
        expose-group-by-app = true;
        # Magnified icon size on hover.  Number is between 16 and 128 (both
        # inclusive).
        largesize = 16;
        # Enable spring loading on all dock items.
        enable-spring-load-actions-on-all-items = true;
        # Animate opening applications from the Dock.
        launchanim = true;
        # Magnify icon on hover.
        magnification = false;
        # Set the minimize/maximize window effect.
        mineffect = "genie";
        # Whether to minimize windows into their application icon.
        minimize-to-application = false;
        # Enable highlight hover effect for the grid view of a stack in the
        # Dock.
        mouse-over-hilite-stack = true;
        # Whether to automatically rearrange spaces based on most recent use.
        mru-spaces = true;
        # Position of the dock on screen.
        orientation = "bottom";
        # Show indicator lights for open applications in the Dock.
        show-process-indicators = true;
        # Show recent applications in the dock.  Specifically, show
        # _Applications_ in the Recent Items Apple menu.
        show-recents = false;
        # Whether to make icons of hidden applications tranclucent.
        showhidden = false;
        # Show only open applications in the Dock.
        static-only = false;
        # Size of the icons in the dock.
        tilesize = 44;
        # The following are hot corner actions for the 4 corners.  All options
        # for them are the same.  Valid values include:
        # 1: Disabled
        # 2: Mission Control
        # 3: Application Windows
        # 4: Desktop
        # 5: Start Screen Saver
        # 6: Disable Screen Saver
        # 7: Dashboard
        # 10: Put Display to Sleep
        # 11: Launchpad
        # 12: Notification Center
        # 13: Lock Screen
        # 14: Quick Note
        #
        # Bottom left.
        wvous-bl-corner = null;
        wvous-br-corner = null;
        wvous-tl-corner = null;
        wvous-tr-corner = null;
      };
      finder = {
        # Some of these are repeats from the NSGlobalDomain.  I should tie them
        # together programmatically, but for now they are just duplicated to be
        # the same.
        #
        # Whether to always show file extensions.
        AppleShowAllExtensions = true;
        # Whether to always show hidden files.
        AppleShowAllFiles = true;
        # Whether to show icons on the desktop or not.
        CreateDesktop = false;
        # Change the default search scope.  Use “SCcf” to default to current
        # folder.  The default is unset (“This Mac”).
        FXDefaultSearchScope = "SCcf";
        FXEnableExtensionChangeWarning
        # Default Finder window set to column view.
        # FXPreferredViewStyle = "clmv";
        # Finder search in current folder by default.
        FXDefaultSearchScope = "SCcf";
        # Disable warning when changing file extension.
        FXEnableExtensionChangeWarning = false;
        # Allow quitting of Finder application
        # QuitMenuItem = true;
      };
      # Disable "Are you sure you want to open" dialog.
      # LaunchServices.LSQuarantine = false;
      # Disable trackpad tap to click.
      trackpad.Clicking = false;
      universalaccess = {
        # Zoom in with Control + Scroll Wheel.
        closeViewScrollWheelToggle = true;
        closeViewZoomFollowsFocus = true;
      };
      # Where to save screenshots.
      screencapture.location = "~/Desktop";
    };
    keyboard = {
      enableKeyMapping = true; # Required to make remapping work.
      remapCapsLockToControl = true;
    };
  };
}
