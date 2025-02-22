{ pkgs, ... }: {
  imports = [
    ../nixos-modules/3d-printing-desktop.nix
    ../nixos-modules/steam-gaming.nix
    ../nixos-modules/timezone-pacific.nix
    ../users/cassandra-desktop.nix
    ../nixos-modules/x-desktop.nix
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
