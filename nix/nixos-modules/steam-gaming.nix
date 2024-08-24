# https://nixos.wiki/wiki/Steam
{ lib, pkgs, ... }: {
  imports = [
    ./unfree-predicates.nix
  ];
  environment.systemPackages = [
    pkgs.steamcmd
  ];
  hardware.opengl = {
    # There can be a version mismatch
    # https://wiki.hyprland.org/Nix/Hyprland-on-NixOS/
    # package = pkgs-unstable.mesa.drivers;

    # if you also want 32-bit support (e.g for Steam).
    driSupport32Bit = true;
    # package32 = pkgs-unstable.pkgsi686Linux.mesa.drivers;
  };
  allowUnfreePackagePredicates = [
    (pkg: builtins.elem (lib.getName pkg) [
      "steam"
      "steamcmd"
      "steamcmd-20180104"
      "steam-original"
      "steam-run"
    ])
  ];
  programs.steam = {
    enable = true;
    # Enables features such as resolution upscaling and stretched aspect ratios
    # (such as 4:3).
    gamescopeSession.enable = true;
    # Open ports in the firewall for Steam Local Network Game Transfers.
    localNetworkGameTransfers.openFirewall = true;
  };
}
