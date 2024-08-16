################################################################################
# Make this host an X server based desktop.
################################################################################
{ ... }: {
  # Hint to Electron apps to use Wayland.
  environment.sessionVariables.NIXOS_OZONE_WL = "1";
  programs.hyprland = {
    # Install the packages from nixpkgs
    enable = true;
    # Whether to enable XWayland
    xwayland.enable = true;
  };
  services.libinput = {
    enable = true;
  };
  services.xserver = {
    enable = true;
  };
}
