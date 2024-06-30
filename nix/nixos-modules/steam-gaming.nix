# https://nixos.wiki/wiki/Steam
{ lib, pkgs, ... }: {
  environment.systemPackages = [
    pkgs.steamcmd
  ];
  programs.steam = {
    enable = true;
    # Enables features such as resolution upscaling and stretched aspect ratios
    # (such as 4:3).
    gamescopeSession.enable = true;
    # Open ports in the firewall for Steam Local Network Game Transfers.
    localNetworkGameTransfers.openFirewall = true;
  };
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "steam"
    "steamcmd"
    "steam-original"
    "steam-run"
  ];
}
