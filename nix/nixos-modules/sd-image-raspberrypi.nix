# This needs to be renamed.  This is for _building_ a Raspberry Pi image, not
# _being_ one.
{ flake-inputs, lib, ... }: let
  format = attr: ({ modulesPath, ... }: {
    imports = [
      "${toString modulesPath}/installer/sd-card/sd-image-raspberrypi.nix"
    ];
    isoImage.squashfsCompression = null;
    # formatAttr = "sdImageRaspberryPi";
    formatAttr = attr;
  });
in {
  imports = [
    flake-inputs.nixos-generators.nixosModules.all-formats
  ];
  formatConfigs.sd-image-raspberrypi = format "sdImage";
  formatConfigs.sdImageRaspberrypi = format "sdImageRaspberryPi";
}
