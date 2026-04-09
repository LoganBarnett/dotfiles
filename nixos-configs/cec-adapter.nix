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

  # Uncomment to cap resolution to 1080p if the inline CEC adapter degrades
  # the 4K HDMI signal (was observed as a black screen at 4K@30Hz / 297 MHz
  # TMDS, but may have been caused by testing on the wrong HDMI port).
  # services.xserver.extraConfig = ''
  #   Section "Monitor"
  #     Identifier "HDMI-2"
  #     Option     "PreferredMode" "1920x1080"
  #     MaxResolution 1920 1080
  #   EndSection
  # '';
}
