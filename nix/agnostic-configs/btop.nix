################################################################################
# Provide fonts that are known to work with btop, and provide btop itself.
# See also ../home-configs/btop.nix for specific configuration of btop.
################################################################################
{ pkgs, ... }: {
  fonts.packages = [
    pkgs.source-code-pro
    # This is a recommended font, but Source Code Pro works too.
    pkgs.nerd-fonts.terminess-ttf
  ];
  environment.systemPackages = [
    pkgs.btop
  ];
}
