##
# Provides settings needed to build a Raspberry Pi 5 bootable image.
##
{ flake-inputs, ... }: {
  modules = [
    flake-inputs.raspberry-pi-nix.nixosModules.raspberry-pi
  ({ pkgs, ... }: {
     # Note: uboot not yet supported for pi5.  See:
     # https://github.com/tstat/raspberry-pi-nix/issues/13#issuecomment-2090601812
     raspberry-pi-nix.uboot.enable = false;
    })
  ];
}
