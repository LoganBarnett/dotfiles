################################################################################
# This includes any and all hardware peripheral settings for Raspberry Pis.  See
# https://wiki.nixos.org/wiki/NixOS_on_ARM/Raspberry_Pi for reference.
################################################################################
{ version, model }:  {
  audio = { ... }: {
    sound.enable = true;
    services.pulseaudio.enable = true;
    boot.loader.raspberryPi.firmwareConfig = ''
    dtparam=audio=on
  '';
  };
  bluetooth = { pkgs, ... }: {
    systemd.services.btattach = {
      before = [ "bluetooth.service" ];
      after = [ "dev-ttyAMA0.device" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.bluez}/bin/btattach -B /dev/ttyAMA0 -P bcm -S 3000000";
      };
    };
  };
  camera = {...}: {
    # Ugh, deprecation warnings that documentation uses.
    # boot.loader.raspberryPi.enable = true;
    # # Set the version depending on your raspberry pi.
    # boot.loader.raspberryPi.version = version;
    # # We need uboot
    # boot.loader.raspberryPi.uboot.enable = true;
    # # These two parameters are the important ones to get the
    # # camera working. These will be appended to /boot/config.txt.
    # boot.loader.raspberryPi.firmwareConfig = ''
    #   start_x=1
    #   gpu_mem=256
    # '';
  };
  serial = {...}: {
    boot.kernelParams = [
      "console=ttyS1,115200n8"
    ];
  };
  # For some fancy video settings?
    # boot.kernelModules = [ "bcm2835-v4l2" ];
}
