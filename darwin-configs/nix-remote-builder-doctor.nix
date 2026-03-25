# Enable nix-remote-builder-doctor on Darwin hosts.
{
  config,
  facts,
  pkgs,
  ...
}:

{
  services.nix-remote-builder-doctor = {
    enable = true;
    builders = [
      {
        name = "silicon";
        hostName = "silicon.${facts.network.domain}";
      }
      {
        name = "rpi-build";
        hostName = "rpi-build.${facts.network.domain}";
      }
    ];
  };
}
