##
# Manage my various macOS settings here.  These would be typically configured
# via preference panes and the like, but Nix can actually handle quite a bit of
# this.
#
# See the following sources for configuration values availble:
# - https://macos-defaults.com
# - https://gist.github.com/mkhl/455002#file-ctrl-f1-c-L12
#
# Also use these invocations to detect settings made in the UI:
# Search for a known value.  Prints the file name so you can see what file it
# resides within.
# ls ~/Library/Preferences | xargs -I{} bash -c 'echo {} ; defaults read ~/Library/Preferences/{}' | grep AppleDictationAutoEnable
# Search for a known value with added context.  Prints the file name so you can
# see what file it resides within.
# ls ~/Library/Preferences | xargs -I{} bash -c 'echo {} ; defaults read ~/Library/Preferences/{}' | grep -B10 AppleDictationAutoEnable
# Before capture, includes file name in output:
# ls ~/Library/Preferences | xargs -I{} bash -c 'echo {} ; defaults read ~/Library/Preferences/{}' > dictation-after.txt
# After capture, includes file name in output:
# ls ~/Library/Preferences | xargs -I{} bash -c 'echo {} ; defaults read ~/Library/Preferences/{}' > dictation-before.txt
# The number may need to be bigger to capture additional context.
# diff dictation-before2.txt dictation-after2.txt --color -C10
# Be sure to capture the before-state first!  Surprising variables can change.
# The invocations will need adaptation for your needs as well, and might need
# further study for how to activate with and without user settings.
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

{ config, emacs-overlay, flake-inputs, nixpkgs, lib, pkgs, ... }:
{
  # Global packages that can't be bound to a specific user, such as shells.
  environment = {
    loginShell = pkgs.zsh;
    shells = [ pkgs.zsh ];
    # systemPackages = [
    #   # Read EXIF metadata from images.
    #   pkgs.exiftool
    #   # Handy for moving files around, over SSH or locally.  I think it supports
    #   # FTP as well.  Keep in mind that rsync needs to be installed on both
    #   # sides, or you will get a cryptic "command not found" error.
    #   pkgs.rsync
    #   pkgs.zsh
    # ] ++ (pkgs.callPackage ./general-packages.nix {});
    systemPackages = [
      # pkgs.nightlight
      # I worry macOS ships with a weird version of ssh-keyscan and I've been
      # trying to get Nix to install the GNU version.  `openssh` is the
      # package in nixpkgs and from my poking around in the Nix code I cannot
      # see anything that would exclude it from showing up.  In fact I can
      # see the package in the store with bin/ssh et. al,.  However these do not
      # appear in /run/current-system/sw/bin.  I have also tried placing the
      # pkgs.openssh declaration in the general-packages.nix file but still no
      # joy.
      # pkgs.openssh
    ];
  };
  fonts = {
    fontDir.enable = true;
    fonts = [
      pkgs.source-code-pro
    ];
  };
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };
  # Enable the ability to search packages offline, assuming the index was built
  # ahead of time (need to check if it actually needs Internet access during
  # initial indexing).  The nix-index package also provides a command-not-found
  # helper which is injected into the shell automatically by nix-darwin.
  # To use this, run `nix-index` manually to generate this index.  This takes
  # several minutes.
  #
  # Now how do you actually _use_ this?  Who knows.  The code of
  # command-not-found.sh (not from the command-not-found package, but
  # nix-index) has this:
  # nix-locate --minimal --no-group --type x --type s --top-level --whole-name --at-root "/bin/$cmd"
  nix = {
    # Lack of pluralization is intentional (package vs packages) - it refers to
    # the package "nix", which is the base of Nix.
    package = pkgs.nix;
    # See https://gist.github.com/jmatsushita/5c50ef14b4b96cb24ae5268dab613050
    # for an example of how to make this work across platforms.
    extraOptions = ''
      # Place any extra options in extraOptions that are not supported via
      # nix.settings.
      # Work around 'warning: ignoring untrusted substituter
      # 'https://yoriksar-gh.cachix.org', you are not a trusted user.' per:
      # https://github.com/YorikSar/nixos-vm-on-macos
      # extra-trusted-substituters = https://yoriksar-gh.cachix.org
      # extra-trusted-public-keys = yoriksar-gh.cachix.org-1:YrztCV1unI7qDV6IXmiXFig5PgptqTlUa4MiobULGT8=
    '';
    # https://nixcademy.com/2024/02/12/macos-linux-builder/ states to use this,
    # but it doesn't exist.  Perhaps I need to bump nix-darwin?  The date is
    # _very_ recent.
    settings = {
      # Do not add this, as nix-darwin sets this up for us, and more
      # intelligently.  Specifying this will crush what nix-darwin configures
      # and will harm bootstrapping efforts with the VM.
      # builders = [ "ssh-ng://linux-builder aarch64-linux,x86_64-linux" ];
      experimental-features = [ "nix-command" "flakes" ];
      # extra-platforms = [ "x86_64-linux" "i686-linux" ];
      # Trust my user so we can open SSH on port 22 for using the Nix builder.
      # It cannot be overridden as of 2024-02-18.  This is demanded in
      # https://nixos.org/manual/nixpkgs/unstable/#sec-darwin-builder but
      # explained here:
      # https://github.com/Gabriella439/macos-builder?tab=readme-ov-file
      extra-trusted-users = [ "logan" ];
      # Trusting @admin is demanded by the darwin.linux-builder package.
      trusted-users = [ "@admin" ];
    };
  };
  nixpkgs.overlays = import ./overlays/default.nix ++ [
    # Disabled until I can figure out what's going on with some Emacs packages
    # conflicting with Doom.
	  # emacs-overlay.overlays.default
	];
  # Some packages are not "free". We need to specifically bless those.
  # I had trouble using a real function because the depended functions are
  # hard/impossible to reach from this place. It cannot exist later
  # because setting nixpkgs.config is ignored if pkgs is set. I found some
  # of those functions declared here:
  # https://github.com/NixOS/nixpkgs/blob/d84cc41f8babd418c295fcbfbd41a1fd4e2adaec/lib/strings.nix#L699
  # This ticket https://github.com/nix-community/home-manager/issues/2954
  # talks about the issue directly, but never comes to a workable
  # resolution for the allowUnfreePredicate value, which  needs "lib" to
  # work. I'm not familiar enough with nix's structure to really move
  # forward here. What we have now is the equivalent of what's commented
  # below:
  # config.allowUnfree = true;
  nixpkgs.config.allowUnfreePredicate = (pkg: true);
  # This has been needed to individually bless some older packages, such
  # as packages depending upon an older OpenSSL.
  nixpkgs.config.permittedInsecurePackages = [];
  # nixpkgs.legacyPackages.${system};
  # I haven't fully tested this, but it doesn't lay down
  # ~/.nix-profile/share/emacs/site-lisp, let alone ~/nix-profile/share/emacs
  # (or the same for the /nix/.../currentsystem/ side).  This means we can't
  # refer to mu4e, or perhaps even built-in packages.  From what I can gather,
  # this just means to refer to mu4e via the load-path that Nix sets up.  From
  # there, mu4e can be found. It _must not_ be listed in packages.el under
  # Doom's package list with straight.el.  This will cause a problem.
  # programs.emacs = {
  #   enable = true;
  #   # package = pkgs.emacs-unstable; # Emacs 29.2.
  #   package = ((pkgs.emacsPackagesFor pkgs.emacs-unstable).emacsWithPackages (
  #   # package = ((pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages (
  #     epkgs: [
  #       epkgs.mu4e
  #     ]
  #   ));
  #   # package = pkgs.emacs;
  #   # package = pkgs.emacs-unstable.pkgs.withPackages (epkgs: [
  #   #   epkgs.melpaPackages.mu4e
  #   # ]);
  #   # extraPackages = let
  #   #   emacsPackages = (pkgs.emacsPackagesNgGen pkgs.emacs).override (final: prev: {
  #   #     mu4e = prev.melpaPackages.mu4e;
  #   #   });
  #   #   in
  #   #     emacsPackages.emacsWithPackages (epkgs: [
  #   #       epkgs.mu4e
  #   #     ])
  #   #     ;
  #   # extraPackages = epkgs: [
  #   #   # Use `melpaPackages` to get the latest version.  Without any attribute
  #   #   # (just `epkgs`), it implies melpa-stable.  See
  #   #   # https://github.com/NixOS/nixpkgs/issues/27083 for more context.
  #   #   # Don't use the supplied `epkgs`, because it doesn't include
  #   #   # `melpaPackages`.
  #   #   pkgs.emacsPackages.melpaPackages.mu4e
  #   # ];
  #   # extraPackages = epkgs: [
  #   #   epkgs.mu4e
  #   # ];
  # };
  programs.nix-index.enable = true;
  security.pam.enableSudoTouchIdAuth = true;
  # Without this, nothing works.
  services.nix-daemon.enable = true;
  system = {
    # Settings that don't have an option in nix-darwin.
    activationScripts.postActivation.text = ''
      # `grep` doesn't work well with `set -o pipefail` because it returns exit
      # code 1 if there are no matches.  We don't care about that, so make sure
      # we zero-ize a 1.  Other errors (2+) are real errors we should pay
      # attention to though.
      function greppipe {
        grep "$@" || test $? = 1;
      }
      # Note that a lot of advice out there will say to use "trustAsRoot" per
      # newer versions of macOS.  This might be correct advice in the context of
      # the question given, but since we're running _as root_ already, we can
      # just trustRoot. `trustAsRoot` is for non-root command line invocations.
      security add-trusted-cert \
        -d \
        -r trustRoot \
        -k /Library/Keychains/System.keychain \
        ${./secrets/proton-ca.crt}
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
      echo "Swapping Option + Command keys on external keyboard..."
      # TODO: Move this to its own shell script for easier testing and
      # portability.
      # This will need to be reapplied on a restart.  See about integrating this
      # with the built-in system for nix-darwin, because it likely handles all
      # of that.  See
      # https://apple.stackexchange.com/questions/329085/tilde-and-plus-minus-±-in-wrong-place-on-keyboard/353941#353941
      # for an example of a startup-level setting.  However this means the
      # system needs to login or finish a clean boot first.  Having a global
      # setting is probably better than having a watch-dog process.
      #
      # When remapping for a specific device, that device loses the global
      # mappings.  So we have to reapply them manually.  We use a global setting
      # for for caps->control.  Swapping option and command is desirable for
      # external keyboards (unless they are Apple keyboards).  `hidutil property
      # --help` has a lot of useful information on how to go about this.
      # Warning:  If the device indicated with --matching isn't present, the
      # hidutil will apply the configuration globally and we don't want that.
      # This will cause the command and option keys to be inverted on the
      # onboard laptop keyboard - undesirable.  We must first detect if our
      # keyboard is present, and then proceed to provide mappings if we so
      # choose.  In fact, we should invert for all non-Apple keyboards.  Per
      # https://www.usb.org/sites/default/files/documents/hut1_12v2.pdf
      # keyboards are page ID 07.  I've tried this and 0x07 but they do not
      # work.
      # keyboards=$(hidutil list --matching '{"PrimaryUsagePage":7}')
      # Use `hidutil property --set '{"UserKeyMapping": []}'` to set.
      # echo "Move old key map file out of the way to fix non-save error in hidutil."
      # mv --verbose ~/Library/Preferences/com.apple.symbolichotkeys.plist{,.old}
      keyboards=$(
        hidutil list \
          | greppipe --ignore-case keyboard \
          | greppipe --invert-match 'Apple ' \
          | greppipe AppleUserHIDDevice \
          | tr -s ' ' \
          | cut -d $' ' -f 2 \
          | uniq
      )
      echo "Keyboards found: $keyboards"
      for keyboard in ''${keyboards[@]}; do
        echo "Setting keyboard remaps for $keyboard..."
        hidutil property --matching "{\"ProductID\":$keyboard}" --set '{"UserKeyMapping":
          [
            {
              "HIDKeyboardModifierMappingSrc":0x7000000E3,
              "HIDKeyboardModifierMappingDst":0x7000000E2
            },
            {
              "HIDKeyboardModifierMappingSrc":0x7000000E2,
              "HIDKeyboardModifierMappingDst":0x7000000E3
            },
            {
              "HIDKeyboardModifierMappingSrc":0x700000039,
              "HIDKeyboardModifierMappingDst":0x7000000E0
            }
          ]
        }'
      done
      echo "Do not sleep when on AC power."
      pmset -c sleep 0 # Needs testing - UI not immediately updated.
      echo "Allow apps from anywhere..."
      SPCTL=$(spctl --status 2>&1 || true)
      echo "spctl: $SPCTL"
      if ! [ "$SPCTL" = "assessments disabled" ]; then
        echo "Disabling master assessments..."
        sudo spctl --master-disable
        echo "Disabled master assessments."
      fi
      # This doesn't necessarily make all changes appear, but it'll get a lot of
      # them.
      echo "Invoking activateSettings to make changes stick..."
      /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
      echo "activateSettings finished with $?."
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
      echo "Fixing dictation spam..."
      # Example invocation can be found at:
      # https://zameermanji.com/blog/2021/6/8/applying-com-apple-symbolichotkeys-changes-instantaneously/
      defaults write com.apple.symbolichotkeys.plist AppleSymbolicHotKeys -dict-add 164 "
        <dict>
          <key>enabled</key><false/>
          <key>value</key><dict>
            <key>type</key><string>standard</string>
            <key>parameters</key>
            <array>
              <integer>65535</integer>
              <integer>65535</integer>
              <integer>0</integer>
            </array>
          </dict>
        </dict>
      "
      # The -int part of this command is critical, or the value won't be
      # respected.
      defaults write com.apple.HIToolbox.plist AppleDictationAutoEnable -int 0

      # Example of writing using something from Nix itself:
      # "$(__dock_item \$\{pkgs.slack}/Applications/Slack.app)"

      # Yes this is duplicated, just to be sure.  It completes sub-second
      # anyways.
      /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
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
        "com.apple.sound.beep.volume" = 1.0;
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
        # Default Finder window set to column view.
        # FXPreferredViewStyle = "clmv";
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
  nix.nixPath = [
    "nixos-config=/etc/nixos/configuration.nix"
  ] ++ (lib.mapAttrsToList
    (key: value: "${key}=${value.to.path}")
    (lib.filterAttrs (key: value: value ? to.path) config.nix.registry)
  );
  nix.registry.nixpkgs.flake = flake-inputs.nixpkgs;
}
