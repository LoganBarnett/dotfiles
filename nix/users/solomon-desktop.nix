{ pkgs, ... }: {
  imports = [
    ../nixos-modules/3d-printing-desktop.nix
  ];
  environment.systemPackages = [
    pkgs.firefox
  ];
  # Nothing to do here yet.
  # home-manager.users.solomon = {};
  users.users.solomon = {
    home = "/home/solomon";
    isNormalUser = true;
    initialPassword = "shakingconfusiondistantboundlessviscousrepent";
  };
}
