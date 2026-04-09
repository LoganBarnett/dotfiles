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
# IMPORTANT: HDMI port number must match the physical TV port.
# ============================================================
# The Pulse-Eight adapter cannot auto-detect which HDMI port it is plugged
# into.  It communicates with the host over USB serial (P8_USB protocol) and
# has no access to the TV's EDID/DDC lines, so it relies on a manually
# configured port number.  Kodi stores this in:
#
#   ~/.kodi/userdata/peripheral_data/usb_2548_1002_CEC_Adapter.xml
#     <setting id="cec_hdmi_port" value="3"/>
#
# If this value doesn't match the actual TV HDMI port, the adapter announces
# the wrong CEC physical address and the TV will not route remote control
# commands to it.  Symptoms: CEC device registers in Kodi's log, the TV may
# show the device name, but arrow keys and OK on the TV remote do nothing.
#
# To fix: change cec_hdmi_port in the XML above (or in Kodi's GUI under
# System → Input → Peripherals → CEC Adapter) to match the TV port, then
# restart Kodi.
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
}
