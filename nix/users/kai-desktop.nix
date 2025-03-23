{ config, flake-inputs, lib, pkgs, ... }: {
  allowUnfreePackagePredicates = [
    (pkg: builtins.elem (lib.getName pkg) [
      "blender"
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
    # A Roblox + Wine package.
    # Alas, it's been removed because Roblox deliberately broke the
    # Wine setup:
    # https://www.gamingonlinux.com/2024/03/game-over-for-roblox-on-linux-steam-deck-as-its-now-blocked/
    # pkgs.grapejuice
    # An open source MineCraft launcher with some mod management features. An
    # alternative is prismlauncher but this one sounded more featureful from a
    # quick search.
    pkgs.atlauncher
    pkgs.blender
    pkgs.minetest
    pkgs.firefox
  ];
  # This differs from what we have in nvidia.nix, but TLATER says it's
  # better nowadays:
  # https://discourse.nixos.org/t/help-very-bad-performances-with-gnome-nvidia/36252/9
  # I'd like to prove it here first, then backport it for a test with
  # CUDA compute.
  hardware.nvidia.open = lib.mkForce true;
}
