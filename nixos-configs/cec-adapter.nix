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
# HDMI port number: the adapter cannot auto-detect the physical TV port.
# The port is declaratively set via services.kodi-standalone.peripheralSettings
# below (defaulting to port 1).  Override cec_hdmi_port in the host config
# to match the actual TV HDMI input.
################################################################################
{
  lib,
  pkgs,
  ...
}:
let
  cecDefaults = lib.mapAttrs (_: lib.mkDefault) {
    activate_source = "1";
    button_release_delay_ms = "0";
    button_repeat_rate_ms = "0";
    cec_hdmi_port = "1";
    cec_standby_screensaver = "0";
    cec_wake_screensaver = "1";
    connected_device = "36037";
    device_name = "Kodi";
    device_type = "36051";
    double_tap_timeout_ms = "300";
    enabled = "1";
    pause_or_stop_playback_on_deactivate = "36045";
    pause_playback_on_deactivate = "0";
    physical_address = "ffff";
    power_avr_on_as = "0";
    send_inactive_source = "1";
    standby_devices = "36037";
    standby_devices_advanced = "";
    standby_pc_on_tv_standby = "13011";
    standby_tv_on_pc_standby = "1";
    tv_vendor = "0";
    use_tv_menu_language = "1";
    wake_devices = "36037";
    wake_devices_advanced = "";
  };
in
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

  # Declarative CEC adapter settings.  Both files are managed because Kodi
  # reads one keyed by USB VID:PID and one generic CEC fallback.
  services.kodi-standalone.peripheralSettings = {
    "usb_2548_1002_CEC_Adapter" = cecDefaults;
    "cec_CEC_Adapter" = cecDefaults;
  };
}
