let
  host-id = "scandium";
  system = "aarch64-darwin";
  username = "logan";
in
{ flake-inputs }: {
  imports = [
    ../nixos-modules/nix-builder-consume.nix
    (import ../nixos-modules/sd-image-raspberrypi.nix {
      inherit flake-inputs;
    })
    (import ../nixos-modules/secrets.nix {
      inherit flake-inputs;
      inherit host-id;
    })
    ../nixos-modules/software-engineering-networking.nix
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
          git-name = "Logan Barnett";
          git-signing-key = "41E46FB1ACEA3EF0";
          host-username = username;
        }
      ];
      _module.args.flake-inputs = flake-inputs;
    }
    {
      nixpkgs.hostPlatform = system;
      networking.hostName = host-id;
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
    ({ lib, pkgs, ...}: {
      imports = [
        ../nixos-modules/unfree-predicates.nix
      ];
      allowUnfreePackagePredicates = [
        (pkg: builtins.elem (lib.getName pkg) [
          "ngrok"
          "unrar"
          # It's secure!
          # Can I look at the source to be sure?
          # No!  You just have to trust us.
          # To be honest, I'm not sure why it's unfree when AGPL is listed as
          # the license:
          # https://github.com/signalapp/Signal-Desktop/blob/main/LICENSE
          # It's because it uses Apple's non-redistributable emoji package,
          # which is kind of funny because isn't this redistribution?  See:
          # https://github.com/NixOS/nixpkgs/blob/b5d21ab69d341ff8d06498d3b39b38160533293f/pkgs/by-name/si/signal-desktop/signal-desktop-darwin.nix#L49
          "signal-desktop"
        ])
      ];
      environment.systemPackages = (import ../personal-packages.nix {
        inherit pkgs;
      }) ++ [
        # Note: This might be unusable on nix-darwin, or with flakes.
        pkgs.nixos-option
        # A 3D printer slicer I really like.  It might work for resin printers
        # but I know it best for its FFF/FDM support.
        pkgs.prusa-slicer
        # Yet another chat app.  I guess it's supposed to be secure, but I
        # assume anything going to the Internet is fundamentally insecure to
        # whomever receives it, and everyone in between.
        # There's some other signal packages worth looking at if I get into it
        # enough:
        # https://search.nixos.org/packages?channel=24.11&from=0&size=50&sort=relevance&type=packages&query=signal
        pkgs.signal-desktop
      ];
      system.stateVersion = 5;
    })
  ];
}
