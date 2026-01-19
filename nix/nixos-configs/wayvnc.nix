################################################################################
#
################################################################################
{ config, host-id, lib, pkgs, ... }: {
  # age.secrets."${host-id}-vnc-password" = {
  #   generator.script = "long-passphrase";
  #   mode = "0600";
  #   owner = "logan";
  #   rekeyFile = ../secrets/${host-id}-vnc-password.age;
  # };
  home-manager.users.logan = {
    imports = [
      ../nixos-modules/wayvnc.nix
    ];
    services.wayvnc = {
      enable = true;
      settings = {
        address = "127.0.0.1";
        port = 5902;
      };
    };
  };
  home-manager = {
  # manual.manpages.enable = false;
    useGlobalPkgs = true;
    useUserPackages = true;
    # generateManual.manpages.enable = false;
    # sharedModules = [
    #   {
    #     disabledModules = [
    #       "programs/ashell.nix"
    #       # "programs/codex.nix"
    #     ];
    #     programs.codex.enable = lib.mkForce false;
    #   }
    # ];
  };
  # environment.systemPackages = [ pkgs.ashell ];
}
