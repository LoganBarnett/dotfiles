{ config, flake-inputs, lib, pkgs, ... }: let
  linux-packages = pkgs.linuxPackages_latest;
in {
  allowUnfreePackagePredicates = [
    (pkg: builtins.elem (lib.getName pkg) [
      "minecraft"
      "minecraft-launcher"
    ])
  ];
  imports = [
    ../nixos-modules/unfree-predicates.nix
    ../nixos-modules/cpu-frequency.nix
    # pstate is wrong for me here.  Not sure if this means I should
    # open a ticket.  Perhaps I can open+close one?  See
    # ../nixos-modules/cpu-frequency.nix for more details on this.
    # flake-inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
    flake-inputs.nixos-hardware.nixosModules.common-cpu-amd
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
    pkgs.minetest
    pkgs.firefox
  ];
  # This differs from what we have in nvidia.nix, but TLATER says it's
  # better nowadays:
  # https://discourse.nixos.org/t/help-very-bad-performances-with-gnome-nvidia/36252/9
  # I'd like to prove it here first, then backport it for a test with
  # CUDA compute.
  hardware.nvidia.open = lib.mkForce true;
  hardware.nvidia.package = lib.mkForce linux-packages.nvidiaPackages.stable;
  hardware.graphics.package = lib.mkForce linux-packages.nvidiaPackages.stable;
}
