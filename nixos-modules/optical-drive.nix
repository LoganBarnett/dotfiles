{ config, lib, ... }:
let
  cfg = config.hardware.opticalDrive;
in
{
  options.hardware.opticalDrive = {
    lockDoor = lib.mkOption {
      type = lib.types.nullOr lib.types.bool;
      default = null;
      description = ''
        Whether the optical drive door should be locked.  When null
        (the default), no sysctl is set and the kernel's own default
        applies.  Set to false to allow the physical eject button to
        open the tray without software intervention, or true to
        explicitly lock it.
      '';
    };
  };

  config = lib.mkIf (cfg.lockDoor != null) {
    boot.kernel.sysctl."dev.cdrom.lock" = if cfg.lockDoor then 1 else 0;
  };
}
