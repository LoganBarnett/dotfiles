################################################################################
# Declares the shared serial port option for the Aeotec Z-Stick 7 so that
# multiple consumers (zwave-js-ui, etc.) can reference a single source of truth.
################################################################################
{ lib, ... }:
{
  options.hardware.aeotec-z-stick-7.serialPort = lib.mkOption {
    type = lib.types.str;
    description = ''
      Serial port device path for the Aeotec Z-Stick 7.
    '';
    example = "/dev/serial/by-id/usb-example";
  };
}
