################################################################################
# Install Sober, which is the only way to get Roblox installed on a Linux
# desktop.  Unfortunately the only way to install Sober presently is via
# Flatpak, per the wiki:
# - https://wiki.nixos.org/wiki/Roblox
# - https://wiki.nixos.org/wiki/Sober
################################################################################
{ flake-inputs, ... }: {
  imports =  [
    flake-inputs.flatpaks.nixosModules.default
  ];
  services.flatpak = {
    enable = true;
    remotes = {
      flathub = "https://dl.flathub.org/repo/flathub.flatpakrepo";
    };
    packages = [
      "flathub:app/org.vinegarhq.Sober//stable"
    ];
  };
}
