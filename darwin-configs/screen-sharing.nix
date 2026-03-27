################################################################################
# Enable macOS Screen Sharing (VNC on port 5900).  No nix-darwin option exists
# for this service, so we use an activation script to load the built-in launch
# daemon.  Drop this into any host that needs remote screen access.
################################################################################
{ ... }:
{
  system.activationScripts.screenSharing.text = ''
    echo "Enabling Screen Sharing (VNC)..."
    launchctl enable system/com.apple.screensharing
    launchctl load -w /System/Library/LaunchDaemons/com.apple.screensharing.plist \
      2>/dev/null || true
  '';
}
