################################################################################
# Support for USB HDMI-CEC adapters (e.g. Pulse-Eight).
#
# Enables the kernel CEC subsystem and Pulse-Eight USB driver so the adapter
# appears as /dev/cec0.  Adds userspace tools for debugging (cec-client,
# cec-ctl).  Uses inputattach to bind the USB serial device to the serio
# layer so the pulse8_cec driver can claim it.
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

  boot.kernelModules = [ "pulse8_cec" ];

  environment.systemPackages = [
    # cec-client — interactive CEC console and scanner.
    pkgs.libcec
    # cec-ctl — low-level CEC monitoring and control.
    pkgs.v4l-utils
  ];

  # /dev/ttyACM0 is owned by root:dialout.  libcec uses the serial device as
  # a fallback when the kernel CEC device is unavailable.
  users.groups.dialout = { };

  # The pulse8_cec driver uses the serio subsystem, not USB directly.
  # inputattach bridges the USB serial device (/dev/ttyACM0) to serio so the
  # driver can claim it and create /dev/cec0.
  #
  # A udev rule triggers a templated service whenever a Pulse-Eight adapter
  # appears, so hotplug and boot-time detection both work.
  services.udev.extraRules = ''
    SUBSYSTEM=="tty", ACTION=="add", ATTRS{idVendor}=="2548", ATTRS{idProduct}=="1002", TAG+="systemd", ENV{SYSTEMD_WANTS}="pulse8-cec-attach@%k.service"
  '';

  systemd.services."pulse8-cec-attach@" = {
    description = "Attach Pulse-Eight CEC adapter on %i";
    after = [ "dev-%i.device" ];
    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.linuxConsoleTools}/bin/inputattach --pulse8-cec /dev/%i";
      Restart = "on-failure";
      RestartSec = "3";
    };
  };
}
