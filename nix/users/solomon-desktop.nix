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
      # Restore to 17:30 start time on weekends after today.
      weekend = {
        enableAt = [ "11:30" ];
        logoutAt = [ "12:30" ];
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
