{ disko-proper, flake-inputs }: { ... }: let
  host-id = "titanium";
  system = "x86_64-linux";
in {
  # inherit system;
  # nixpkgs.config.system = system;
  nixpkgs.hostPlatform = system;
  imports = [
    ../nixos-modules/amd-gpu.nix
    (import ../nixos-modules/server-host.nix {
      inherit host-id flake-inputs disko-proper system;
    })
    ../nixos-modules/steam-gaming.nix
  ];
}
