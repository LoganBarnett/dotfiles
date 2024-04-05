let
  system = "aarch64-darwin";
in
{ flake-inputs }: {
  inherit system;
  modules = [
    (import ../nixos-modules/secrets.nix {
      inherit flake-inputs system;
      host-id = "scandium";
      host-public-key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN3jJteck5yCfIm0iA4qKSIVx9zh6qhCuAt5cV1Ysib+";
    })
    flake-inputs.home-manager.darwinModules.home-manager
    {
      nixpkgs.overlays = [ flake-inputs.fenix.overlays.default ];
    }
    # Before I was using a curried function to pass these things in, but
    # the _module.args idiom is how I can ensure these values get passed
    # via the internal callPackage mechanism for darwinSystem on these
    # modules.  We want callPackage because it does automatic "splicing"
    # of nixpkgs to achieve cross-system compiling.  I don't know that we
    # need to use this at this point, but making it all consistent has
    # value.
    {
      _module.args.emacs-overlay = flake-inputs.emacs-overlay;
      _module.args.nixpkgs = flake-inputs.nixpkgs;
    }
    {
      config.networking.hostName = "scandium";
    }
    ../darwin.nix
    ../users/logan-personal.nix
    ../headed-host.nix
    ../darwin-linux-builder-module.nix
    ({ pkgs, ...}: {
      environment.systemPackages = (import ../personal-packages.nix {
        inherit pkgs;
      }) ++ [
        # This should remain out because agenix-rekey brings in agenix - or at
        # least the bits of it we are interested in.
        # flake-inputs.agenix.packages.${system}.default
        pkgs.agenix-rekey
        # Rage is a Rust based Age that claims a more consistent CLI API.
        pkgs.rage
      ];
    })
  ];
}
