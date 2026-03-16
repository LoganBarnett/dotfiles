################################################################################
# Enable realtime game streams (interactive).
#
# Sunshine requires a PIN to complete handshaking, which is done via the desktop
# typically.  However a "local" connection to the web application it hosts is
# also adequate for supplying this PIN.
# 1. Setup an SSH tunnel (e.g. ssh -L 47990:127.0.0.1:47990 titanium.proton),
#    47990 is the default port.
# 2. Navigate to https://localhost:47990.  You may have to accept a self signed
#    certificate.  I would like to find out how to fix that, but it is left
#    another day.
# 3. Create a user account if necessary, and save the credentials somewhere
#    secure.
# 4. Wait or refresh to log in with the newly created credentials.  The UI will
#    instruct you to do this too.
# 5. Once logged in, click on the PIN tab at the top.
# 6. Attempt to connect via Moonlight on the target host (titanium.proton in my
#    case).
# 7. Take the PIN it demands, and input it into the web UI.  Submit.
# 8. Return to Moonlight.  You should see that the connection icon to your host
#    no longer has a lock icon on it.  Double click it to connect.
# 9. Enjoy streaming.
################################################################################
# Capture backend selection (GNOME Wayland)
#
# Sunshine supports several capture backends on Linux.  Those that do NOT work
# here are documented to avoid revisiting them:
#
# wlr-export-dmabuf
#   Only implemented by wlroots compositors (Sway, Hyprland, etc.).  GNOME
#   Shell / Mutter does not expose this protocol.  Sunshine logs:
#   "Warning: Missing Wayland wire for wlr-export-dmabuf"
#
# KMS/DRM  (capSysAdmin = true, with adapter_name)
#   GNOME holds exclusive DRM master while running, so the KMS connector list
#   appears empty regardless of CAP_SYS_ADMIN.  Sunshine logs an empty KMS
#   monitor list, then "Unable to initialize capture method".  Critically,
#   setting adapter_name commits Sunshine to KMS and prevents it from falling
#   through to the portal — adapter_name must NOT be set alongside this config.
#
# X11 / gnome-xorg session  (defaultSession = "gnome-xorg")
#   Was tested; caused significant feature regression.  Not viable.
#
# XWayland capture  (UnsetEnvironment = ["WAYLAND_DISPLAY"] in service)
#   Sunshine connects successfully, but the XWayland root window (:0) is black
#   — it does not mirror Wayland compositor output, only native X11 clients.
#
# XDG Desktop Portal (PipeWire screencast)  ← used here
#   The correct path for GNOME Wayland.  nixpkgs omits pipewire and libportal;
#   adding them causes CMake to detect them via pkg-config and enable the portal
#   screencast code path.  Sunshine requests a screencast session via
#   org.freedesktop.portal.ScreenCast; GNOME shows a one-time permission dialog
#   and then streams frames over PipeWire.
#
#   Limitation: GNOME denies portal screencasts while the session is locked.
#   See the unlock-session service below for the workaround.
################################################################################
{ lib, pkgs, ... }:
{
  services.sunshine = {
    enable = true;
    openFirewall = true;
    capSysAdmin = true;
    # nixpkgs omits pipewire and libportal; adding them here causes CMake to
    # detect and enable the XDG portal screencast capture path, which is the
    # only working capture method on GNOME Wayland.
    package = pkgs.sunshine.overrideAttrs (old: {
      buildInputs = old.buildInputs ++ [
        pkgs.pipewire
        pkgs.libportal
      ];
    });
  };

  # Auto-login starts the graphical session so Sunshine is always reachable via
  # Moonlight after a reboot.  The lock service immediately locks the screen, so
  # physical access to a running machine still requires the password — the
  # security model is equivalent to a locked, unattended desktop.
  services.displayManager.autoLogin = {
    enable = true;
    user = "logan";
  };
  services.displayManager.defaultSession = "gnome";
  systemd.user.services.lock-on-login = {
    description = "Lock screen immediately after auto-login";
    wantedBy = [ "graphical-session.target" ];
    after = [ "graphical-session.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.systemd}/bin/loginctl lock-session";
    };
  };

  # Companion to lock-on-login.  Start this over SSH to restore portal
  # capture access before connecting via Moonlight:
  #   systemctl --user start unlock-session
  systemd.user.services.unlock-session = {
    description = "Unlock screen to restore Sunshine capture access";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.systemd}/bin/loginctl unlock-session";
    };
  };

}
