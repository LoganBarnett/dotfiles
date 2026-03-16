{ pkgs, ... }: {
  programs.lutris = {
    enable = true;
    # Lutris wants to download its own Wine packages, which will have linking
    # problems with Nix.  Fortunately the module maintainers have thought of a m
    # means of providing Wine to Lutris.
    winePackages = [
      pkgs.wineWowPackages.stable
      pkgs.wineWowPackages.staging
      pkgs.wineWowPackages.waylandFull
      # pkgs.wineWowPackages.full
    ];
  };
}
