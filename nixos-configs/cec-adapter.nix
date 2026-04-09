################################################################################
# Support for USB HDMI-CEC adapters (e.g. Pulse-Eight).
#
# Kodi's libcec talks directly to the Pulse-Eight adapter over the USB serial
# device (/dev/ttyACM0) using its P8_USB protocol.  This is simpler and
# better-supported than the kernel CEC framework path (inputattach → serio →
# pulse8_cec → /dev/cec0), which conflicts with libcec's exclusive serial
# access.
#
# The kernel CEC subsystem is still enabled for the i915 DRM driver, but the
# pulse8_cec module and inputattach service are intentionally omitted.
#
# TEMPORARY — RETURNING CURRENT HARDWARE
# =======================================
# The current Pulse-Eight USB-CEC adapter (inline pass-through model) degrades
# the HDMI signal at 4K@30Hz (297 MHz TMDS).  The Mac Mini 7,1's HDMI 1.4
# port is already at the bandwidth ceiling, and the adapter's extra connector
# pair pushes the signal below the TV's lock threshold — resulting in a black
# screen.  1080p@60Hz (148.5 MHz) works fine through the adapter.
#
# The resolution is capped at 1080p below until a replacement adapter arrives.
# The fix is to use an adapter that does NOT sit inline on the TMDS lines —
# either the same Pulse-Eight in non-inline mode (separate HDMI port, physical
# address override in libcec) or a Pi Zero W CEC bridge.
#
# When the new adapter is installed, remove the services.xserver.extraConfig
# block below to restore 4K output.
################################################################################
{
  pkgs,
  ...
}:
{
  environment.systemPackages = [
    # cec-client — interactive CEC console and scanner.
    pkgs.libcec
    # cec-ctl — low-level CEC monitoring and control.
    pkgs.v4l-utils
  ];

  # /dev/ttyACM0 is owned by root:dialout.  The kodi user needs this group
  # to open the serial device.
  users.groups.dialout = { };

  # TEMPORARY: Cap resolution to 1080p while the current inline CEC adapter
  # degrades the 4K HDMI signal.  Remove this block when the adapter is
  # replaced.
  services.xserver.extraConfig = ''
    Section "Monitor"
      Identifier "HDMI-2"
      Option     "PreferredMode" "1920x1080"
      MaxResolution 1920 1080
    EndSection
  '';
}
