{ flake-inputs }: let
  hostname = "M-CL64PK702X";
  system = "aarch64-darwin";
in {
  inherit system;
  modules = [
    (import ../nixos-modules/secrets.nix {
      inherit flake-inputs system;
      host-id = hostname;
      host-public-key = "";
    })
    flake-inputs.home-manager.darwinModules.home-manager
    {
      nixpkgs.overlays = [ flake-inputs.fenix.overlays.default ];
    }
    # the _module.args idiom is how I can ensure these values get passed via the
    # internal callPackage mechanism for darwinSystem on these modules.  We want
    # callPackage because it does automatic "splicing" of nixpkgs to achieve
    # cross-system compiling.  I don't know that we need to use this at this
    # point, but making it all consistent has value.
    {
			_module.args.emacs-overlay = flake-inputs.emacs-overlay;
      _module.args.nixpkgs = flake-inputs.nixpkgs;
    }
    {
      config.networking.hostName = hostname;
    }
    ../darwin.nix
    ../users/logan-new-e-ah.nix
    ../headed-host.nix
    ({ pkgs, ...}: let
    in {
      security.pki.certificateFiles = [
        "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
        ../new-e-ah-certs.pem
      ];
      environment.systemPackages = [
        pkgs.awscli
        (pkgs.callPackage ../../nix-gems/hiera-eyaml/default.nix {})
      ];
    })
  ];
}