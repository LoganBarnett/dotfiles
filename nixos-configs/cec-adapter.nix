################################################################################
# Support for USB HDMI-CEC adapters (e.g. Pulse-Eight).
#
# Enables the kernel CEC subsystem and Pulse-Eight USB driver so the adapter
# appears as /dev/cec0.  Adds userspace tools for debugging (cec-client,
# cec-ctl).
################################################################################
{
  lib,
  pkgs,
  ...
}:
{
  # The default NixOS kernel ships with CONFIG_MEDIA_CEC_SUPPORT disabled,
  # which gates all USB/platform CEC drivers including pulse8_cec.
  boot.kernelPatches = [
    {
      name = "cec-usb-pulse8";
      patch = null;
      extraConfig = ''
        MEDIA_CEC_SUPPORT y
        USB_PULSE8_CEC m
      '';
    }
  ];

  environment.systemPackages = [
    # cec-client — interactive CEC console and scanner.
    pkgs.libcec
    # cec-ctl — low-level CEC monitoring and control.
    pkgs.v4l-utils
  ];

  # /dev/ttyACM0 is owned by root:dialout.  libcec uses the serial device as
  # a fallback when the kernel CEC device is unavailable.
  users.groups.dialout = { };
}
