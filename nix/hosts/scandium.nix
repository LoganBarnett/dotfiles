let
  host-id = "scandium";
  system = "aarch64-darwin";
  username = "logan";
in
{ flake-inputs, lib, system, pkgs, ... }: let
  nextcloud = (flake-inputs.nextcloud-desktop.packages.${system}.default.overrideAttrs (old: {
    # Inkscape dies with SIGTRAP and we see no other useful information.
    # Sounds like a project unto itself.  However as of
    # https://github.com/nextcloud/desktop/pull/3719, we can use
    # rsvg-convert instead, which should express less desktop-isms that
    # are probably causing Inkscape to die here.
    nativeBuildInputs = (
      lib.lists.filter
        (p: !(lib.traceVal (lib.strings.hasPrefix "inkscape" p.name)))
        old.nativeBuildInputs
    ) ++ [ pkgs.librsvg ]
      # Give us xcodebuild, or some equivalent.
    ++ [ pkgs.xcbuild pkgs.apple-sdk ]
    ;
    buildInputs = old.buildInputs ++ [ pkgs.xcbuild pkgs.apple-sdk ];
    # cmakeFlags = [
    #   "-DCMAKE_OSX_SYSROOT=$(xcrun --sdk macosx --show-sdk-path)"
    # ];
  #   buildInputs = old.buildInputs ++ [ pkgs.libp11 pkgs.libsForQt5 ];
  }));
  pkgs-latest = import flake-inputs.nixpkgs-latest {
    inherit system;
    # TODO: Constrain to signal-desktop-bin to make this precise.  This is
    # needed to be done separately because each pkgs gets its own unfree
    # configuration.
    config.allowUnfree = true;
    # Test it!
    config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
      "signal-desktop-bin"
    ];

  };

in {
  system.primaryUser = username;
  imports = [
    ../nixos-modules/nix-builder-consume.nix
    ../nixos-modules/sd-image-raspberrypi.nix
    ../nixos-modules/secrets.nix
    ../nixos-modules/software-engineering-networking.nix
    ../nixos-modules/wireguard-client-standard.nix
    flake-inputs.home-manager.darwinModules.home-manager
    # Before I was using a curried function to pass these things in, but
    # the _module.args idiom is how I can ensure these values get passed
    # via the internal callPackage mechanism for darwinSystem on these
    # modules.  We want callPackage because it does automatic "splicing"
    # of nixpkgs to achieve cross-system compiling.  I don't know that we
    # need to use this at this point, but making it all consistent has
    # value.
    {
      _module.args.git-users = [
        {
          git-email = "logustus@gmail.com";
          git-name = "Logan Barnett";
          git-signing-key = "41E46FB1ACEA3EF0";
          host-username = username;
        }
      ];
    }
    {
      nixpkgs.hostPlatform = system;
      networking.hostName = host-id;
    }
    ../nixos-modules/tls-trust.nix
    ../nixos-modules/user-can-admin.nix
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
        ])
      ];
      environment.systemPackages = (import ../personal-packages.nix {
        inherit pkgs;
      }) ++ [
        (pkgs.callPackage ../derivations/dice-roller.nix {})
        # Let us communicate with the Matrix chat protocol.
        pkgs.element-desktop
        # Note: This might be unusable on nix-darwin, or with flakes.
        pkgs.nixos-option
        # A 3D printer slicer I really like.  It might work for resin printers
        # but I know it best for its FFF/FDM support.
        (pkgs.prusa-slicer.override { boost = pkgs.boost179; })
        # Allow generating Nextcloud plugins as a Nix expression.
        pkgs.nc4nix
        # nextcloud
        # Yet another chat app.  I guess it's supposed to be secure, but I
        # assume anything going to the Internet is fundamentally insecure to
        # whomever receives it, and everyone in between.
        # There's some other signal packages worth looking at if I get into it
        # enough:
        # https://search.nixos.org/packages?channel=24.11&from=0&size=50&sort=relevance&type=packages&query=signal
        # I'd love to build from source, but the signal-desktop package isn't
        # configured well for overriding.  Presently it is set to be Linux only,
        # and imports a couple of packages using callPackage in a let binding.
        # I tried overriding them where they are used (via passthru), but still
        # no joy.
        pkgs-latest.signal-desktop-bin
      ];
      system.stateVersion = 5;
    })
  ];
}
