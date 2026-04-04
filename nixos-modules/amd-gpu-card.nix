# Declares the DRM card index for the AMD GPU so that configs can
# reference a single source of truth instead of hardcoding card numbers.
{ config, lib, ... }:
let
  cfg = config.hardware.amdGpuCard;
in
{
  options.hardware.amdGpuCard = {
    index = lib.mkOption {
      type = lib.types.ints.unsigned;
      default = 0;
      description = ''
        DRM card index for the AMD GPU (e.g. 0 for /dev/dri/card0, 1 for
        /dev/dri/card1).  Used by X11 modesetting, sonify-health drone
        metrics, and anything else that needs the GPU sysfs path.
      '';
    };

    sysfsPath = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = "/sys/class/drm/card${toString cfg.index}/device";
      description = "Derived sysfs device path for the AMD GPU.";
    };
  };
}
