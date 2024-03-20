{ emacs-overlay, fenix, home-manager, nixpkgs }: {
  system = "aarch64-darwin";
  modules = [
    home-manager.darwinModules.home-manager
    {
      nixpkgs.overlays = [ fenix.overlays.default ];
    }
    # Before I was using a curried function to pass these things in, but
    # the _module.args idiom is how I can ensure these values get passed
    # via the internal callPackage mechanism for darwinSystem on these
    # modules.  We want callPackage because it does automatic "splicing"
    # of nixpkgs to achieve cross-system compiling.  I don't know that we
    # need to use this at this point, but making it all consistent has
    # value.
    {
      _module.args.emacs-overlay = emacs-overlay;
      _module.args.nixpkgs = nixpkgs;
    }
    ../darwin.nix
    ../users/logan-personal.nix
    ../headed-host.nix
    ../darwin-linux-builder-module.nix
    ({ pkgs, ...}: {
      environment.systemPackages = import ../personal-packages.nix {
        inherit pkgs;
      };
    })
  ];
}
