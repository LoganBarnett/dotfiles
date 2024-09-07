{ lib, pkgs, ... }: {
  allowUnfreePackagePredicates = [
    (pkg: builtins.elem (lib.getName pkg) [
      "minecraft"
      "minecraft-launcher"
    ])
  ];
  imports = [
    ../nixos-modules/unfree-predicates.nix
  ];
  # Nothing to do here yet.
  # home-manager.users.kai = {};
  users.users.kai = {
    home = "/home/kai";
    isNormalUser = true;
    initialPassword = "appetizerrelatedabnormalbaboonpostalanime";
  };
  environment.systemPackages = [
    # An open source MineCraft launcher with some mod management features. An
    # alternative is prismlauncher but this one sounded more featureful from a
    # quick search.
    pkgs.atlauncher
    pkgs.minetest
  ];

}
