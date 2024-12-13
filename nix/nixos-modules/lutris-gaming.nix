################################################################################
# Lutris provides games via various platforms (and no platforms).  It can help
# get things going with Wine and a variety of other tools as part of a launcher.
# I think it can do ROMs as well.
################################################################################
{ lib, pkgs, ... }: {

  environment.systemPackages = [
    pkgs.lutris
    pkgs.wineWowPackages.waylandFull
  ];
  hardware.graphics = {
    enable = true;
    # enable32Bit = true;
  };
}
