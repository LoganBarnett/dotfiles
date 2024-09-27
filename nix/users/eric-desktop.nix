{ pkgs, ... }: {
  environment.systemPackages = [
    pkgs.firefox
    pkgs.steam
  ];
  # Nothing to do here yet.
  # home-manager.users.eric = {};
  users.users.eric = {
    home = "/home/eric";
    isNormalUser = true;
    initialPassword = "unworn snagged repulsive detergent onlooker barricade";
  };
}
