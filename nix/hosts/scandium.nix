let
  host-id = "scandium";
  system = "aarch64-darwin";
  username = "logan";
in
{ flake-inputs }: {
  inherit system;
  modules = [
    ../nixos-modules/nix-builder-consume.nix
    (import ../nixos-modules/sd-image-raspberrypi.nix {
      inherit flake-inputs;
    })
    (import ../nixos-modules/secrets.nix {
      inherit flake-inputs;
      inherit host-id;
    })
    flake-inputs.home-manager.darwinModules.home-manager
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
      _module.args.git-users = [
        {
          git-email = "logustus@gmail.com";
          git-username = "LoganBarnett";
          git-signing-key = "41E46FB1ACEA3EF0";
          host-username = username;
        }
      ];
      _module.args.flake-inputs = flake-inputs;
    }
    {
      config.networking.hostName = host-id;
    }
    ../nixos-modules/tls-trust.nix
    (import ../nixos-modules/user-can-admin.nix {
      inherit flake-inputs system;
    })
    (import ../nixos-modules/user-can-develop.nix {
      inherit username;
    })
    ../darwin.nix
    ../users/logan-personal.nix
    ../headed-host.nix
    ../darwin-linux-builder-module.nix
    ({ pkgs, ...}: {
      environment.systemPackages = (import ../personal-packages.nix {
        inherit pkgs;
      });
    })
  ];
}
