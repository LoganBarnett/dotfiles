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
  services.userLockoutSchedule.users.solomon = {
    schedule = let
      weekday = {
        enableAt = [ "18:30" ];
        logoutAt = [ "19:30" ];
      };
      weekend = {
        enableAt = [ "15:00" ];
        logoutAt = [ "16:00" ];
      };
    in {
      mon = weekday;
      tue = weekday;
      wed = weekday;
      thu = weekday;
      fri = weekday;
      sat = weekend;
      sun = weekend;
    };
  };
  # Nothing to do here yet.
  # home-manager.users.solomon = {};
  users.users.solomon = {
    home = "/home/solomon";
    isNormalUser = true;
    initialPassword = "shakingconfusiondistantboundlessviscousrepent";
  };
}
