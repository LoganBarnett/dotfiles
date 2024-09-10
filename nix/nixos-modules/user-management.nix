{ pkgs, ... }: {
  environment.systemPackages = [
    # pkgs.gksudo
    pkgs.xorg.xhost
  ];
}
