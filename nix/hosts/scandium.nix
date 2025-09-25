let
  host-id = "scandium";
  system = "aarch64-darwin";
  username = "logan";
in
{ flake-inputs, lib, system, pkgs, ... }: let
  work-alias = lib.strings.concatStrings (lib.lists.reverseList [
    "a"
    "e"
    "w"
    "n"
  ]);
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
      "zoom-us"
    ];
  };

in {
  system.primaryUser = username;
  # Something required for every macOS host after a nix-darwin migration.  This
  # value will be different per host.  Perhaps hosts stood up after that point
  # won't need it.
  ids.gids.nixbld = 350;
  imports = [
    ../agnostic-configs/logan-workstation.nix
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
    ../darwin-configs/sytter.nix
    ../nixos-modules/tls-trust.nix
    ../nixos-modules/user-can-admin.nix
    (import ../nixos-modules/user-can-develop.nix {
      inherit username;
    })
    ../darwin.nix
    ../users/logan-personal.nix
    ../headed-host.nix
    # Turn this on if I can't use rpi-build.proton for some reason.
    # ../darwin-linux-builder-module.nix
    {
      # Sometimes we run into DNS issues locally, so provide this as an escape
      # hatch.
      environment.etc."ssh/ssh_config.d/102-nickel-escape-hatch.conf".text = ''
        Host nickel.proton
          HostName 192.168.254.1
       '';
      environment.etc."ssh/ssh_config.d/103-${work-alias}-workstation.conf".text = ''
        Host M-CL64PK702X
          User logan.barnett
        Host M-CL64PK702X.proton
          User logan.barnett
        Host m-cl64pk702x
          User logan.barnett
        Host m-cl64pk702x.proton
          User logan.barnett
      '';
    }
    ({ lib, pkgs, ...}: {
      imports = [
        ../nixos-modules/unfree-predicates.nix
      ];
      allowUnfreePackagePredicates = [
        (pkg: builtins.elem (lib.getName pkg) [
          "claude-code"
          "ngrok"
          "unrar"
        ])
      ];
      environment.systemPackages = (import ../personal-packages.nix {
        inherit pkgs;
      }) ++ [
        # Use latest to benefit from work done here:
        # https://github.com/Aider-AI/aider/issues/2318
        pkgs-latest.aider-chat
        # Make machines write the code instead.  What could go wrong? :D
        pkgs-latest.claude-code
        (pkgs.callPackage ../derivations/dice-roller.nix {})
        # Let us communicate with the Matrix chat protocol.
        pkgs.element-desktop
        # Drawing program (Like MS Paint, or more like Gimp/Photoshop?).
        # Linux-only though in Nix.  Probably due to problems with GTK that a
        # lots of Linux GUI-centric programs have in the Nix ecosystem.
        # pkgs.mypaint
        # Note: This might be unusable on nix-darwin, or with flakes.
        pkgs.nixos-option
        # Allow generating Nextcloud plugins as a Nix expression.
        pkgs.nc4nix
        # nextcloud
        # A 3D printer slicer I really like.  It might work for resin printers
        # but I know it best for its FFF/FDM support.
        (pkgs.prusa-slicer.override { boost = pkgs.boost179; })
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
        pkgs.signal-desktop-bin
        # Screen Sharing.app is nice that it's built-in but it doesn't support
        # as many encryption/security options, It think.
        # Doh, broken.
        # pkgs-latest.tigervnc
        # pkgs-latest.turbovnc
        pkgs.moonlight-qt
        # Translate audio to text, but do it fast (unlike Python versions).
        pkgs.whisper-cpp
        pkgs-latest.zoom-us
      ];
      system.stateVersion = 5;
    })
  ];
}
