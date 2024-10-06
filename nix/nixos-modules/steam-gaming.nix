# https://nixos.wiki/wiki/Steam
{ lib, pkgs, ... }: {
  imports = [
    ./unfree-predicates.nix
  ];
  environment.systemPackages = [
    pkgs.steamcmd
  ];
  hardware.graphics = {
    enable = true;
    # 32 bit support is required for Steam games.
    # driSupport32Bit = true;
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
    remotePlay.openFirewall = true;
  };
}
