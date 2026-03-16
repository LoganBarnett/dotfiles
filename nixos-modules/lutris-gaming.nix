################################################################################
# Lutris provides games via various platforms (and no platforms).  It can help
# get things going with Wine and a variety of other tools as part of a launcher.
# I think it can do ROMs as well.
#
# See also ../home-configs/lutris-gaming.nix for additional configuration, such
# as providing Lutris with additional versions of Wine.
################################################################################
{ lib, pkgs, ... }: {

  environment.systemPackages = [
    # Provide DirectX libraries... emulation?
    pkgs.dxvk
    pkgs.dxvk_2
    pkgs.lutris
    # pkgs.wineWowPackages.waylandFull
    pkgs.wineWowPackages.stable
    pkgs.winetricks
    pkgs.protonup-qt
    # pkgs.vkd3d
    pkgs.vulkan-tools
    pkgs.vulkan-loader
    pkgs.vulkan-validation-layers
  ];
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };
  # programs.lutris.enable = true;
}
