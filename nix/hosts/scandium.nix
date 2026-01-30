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
  pkgs-openscad-bin = import flake-inputs.nixpkgs-openscad-bin {
    inherit system;
  };
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
    ../nixos-modules/nix-builder-consume.nix
    ../nixos-configs/nix-store-tools.nix
    ../nixos-modules/sd-image-raspberrypi.nix
    ../nixos-modules/secrets.nix
    ../nixos-modules/software-engineering-networking.nix
    ../nixos-modules/wireguard-client-standard.nix
    ../nixos-configs/workstation.nix
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
      nixpkgs.config.allowUnsupportedSystem = true;
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
    ({ flake-inputs, lib, pkgs, ...}: {
      imports = [
        ../nixos-modules/unfree-predicates.nix
        flake-inputs.nix-homebrew.darwinModules.nix-homebrew
      ];
      allowUnfreePackagePredicates = [
        (pkg: builtins.elem (lib.getName pkg) [
          "firefox-bin"
          "firefox"
          "firefox-bin-unwrapped"
          "ngrok"
          "signal-desktop-bin"
          "unrar"
        ])
      ];
      home-manager.users.logan = {
        imports = [
          ../home-configs/ssh-config-general.nix
          ../home-configs/ssh-config-container-vm.nix
          ../home-configs/ssh-config-emacs.nix
          ../home-configs/ssh-config-proton.nix
        ];
        # Make machines write the code instead.  What could go wrong? :D
        programs.claude-code = {
          # This is a custom setting provided by ../home-modules/claude.nix.
          # For shared settings between various hosts, see
          # ../home-configs/claude-code.nix.
          # Actually we don't need this anymore.
          # passApiKey = "claude-code-api-key";
        };
      };
      # Enable Homebrew integration.
      nix-homebrew = {
        enable = true;
        # If an existing homebrew is in the way, we just intelligently replace
        # it.  This should retain everything that was installed via Homebrew,
        # but only impacts Homebrew itself.
        autoMigrate = true;
        # Uh, what?  What if I need more than one user?
        user = "logan";
        # enableRosetta = true;  # if you want x86_64 brews too
      };
      homebrew = {
        enable = true;
        casks = [
          "alfred"
          "dash"
          "discord"
          # The firefox-bin package doesn't work on macOS - it immediately
          # crashes.
          "firefox"
          # "doxie"
          "istat-menus"
          "keycastr"
          # "openscad"
          "prusaslicer"
          "steam"
          "ultimaker-cura"
          # "vlc"
        ];

        # Optional but recommended:
        # auto-cleanup bottles/casks that are no longer declared.
        onActivation.autoUpdate = false;
        onActivation.cleanup = "uninstall";       # removes undeclared brews/casks
        onActivation.upgrade = false;              # upgrade on switch
      };
      environment.systemPackages = (import ../personal-packages.nix {
        inherit pkgs;
      }) ++ [
        # Use latest to benefit from work done here:
        # https://github.com/Aider-AI/aider/issues/2318
        pkgs-latest.aider-chat
        # A less dial-home-to-an-ad-company way of running Chrome extensions.
        # Not working because it's a Linux only build.
        # pkgs.ungoogled-chromium
        # Should play music of any of the screamer/tracker formats, but doesn't
        # build on macOS because Reasons.
        # pkgs.deadbeef
        (pkgs.callPackage ../derivations/dice-roller.nix {})
        # Let us communicate with the Matrix chat protocol.
        pkgs.element-desktop
        # Download content from Fanbox.
        pkgs.fanbox-dl
        # Crashes on open.
        # pkgs.firefox-bin
        pkgs.moonlight-qt
        # Drawing program (Like MS Paint, or more like Gimp/Photoshop?).
        # Linux-only though in Nix.  Probably due to problems with GTK that a
        # lots of Linux GUI-centric programs have in the Nix ecosystem.
        # pkgs.mypaint
        # Note: This might be unusable on nix-darwin, or with flakes.
        pkgs.nixos-option
        # Allow generating Nextcloud plugins as a Nix expression.
        pkgs.nc4nix
        # nextcloud
        pkgs-openscad-bin.openscad
        # Used as a better `screen` for communicating at various baud rates and
        # formats.  I found this necessary for interfacing with an Aruba managed
        # switch.
        pkgs.picocom
        # Download content from Pixiv.
        pkgs.pxder
        # A 3D printer slicer I really like.  It might work for resin printers
        # but I know it best for its FFF/FDM support.
        # pkgs.prusa-slicer
        # (pkgs.prusa-slicer.override { boost = pkgs.boost179; })
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
        # (pkgs.signal-desktop-bin.overrideAttrs (old: {
        #   preInstall = pkgs.lib.traceVal ''
        #     mkdir src-modified
        #     ls -al "Signal.app"
        #     ${pkgs.asar}/bin/asar \
        #       extract \
        #       "Signal.app/Contents/Resources/app.asar" \
        #       src-modified
        #     ls -al src-modified/app
        #     # Keep this around in case the brittle substitution breaks.
        #     grep -C20 -R hasExpired src-modified/ts/state/selectors/expiration.js
        #     # Let's just see what's inside.
        #     substituteInPlace \
        #       src-modified/ts/state/selectors/expiration.js \
        #       --replace-fail '(buildExpiration, autoDownloadUpdate, now) => {' \
        #         '(buildExpiration, autoDownloadUpdate, now) => { return false;'
        #     # Let's see how the file looks now too.
        #     grep -C20 -R hasExpired src-modified/ts/state/selectors/expiration.js
        #     cd src-modified
        #     ls -al .
        #     # This prevents the V8 engine from barfing on a cache mismatch.
        #     rm -f preload.bundle.cache
        #     ${pkgs.asar}/bin/asar pack . ../app.asar
        #     cd ..
        #     mv app.asar Signal.app/Contents/Resources/app.asar
        #     # exit 1
        #   '';
        #   # installPhase = ''
        #   #   runHook preInstall

        #   #   mkdir -p $out/Applications
        #   #   cp -r Signal.app $out/Applications

        #   #   runHook postInstall
        #   # '';
        # }))
        # (pkgs-latest.signal-desktop-bin.overrideAttrs (old: {
        #   src = pkgs.fetchFromGitHub {
        #     owner = "LoganBarnett";
        #     repo = "Signal-Desktop";
        #     rev = "remove-expiration";
        #     hash = "sha256-tmxaupVwN8k9ZYtFZjDJuhN9bbkIpcWEJ2JDfrDlBgg=";
        #   };
        # }))
        # A cloud VPN provider.  It breaks my self hosted proclivities, but
        # others can give me links to go into their VPNs.
        pkgs.tailscale
        # Screen Sharing.app is nice that it's built-in but it doesn't support
        # as many encryption/security options, It think.
        # Doh, broken.
        # pkgs-latest.tigervnc
        # pkgs-latest.turbovnc
        # Translate audio to text, but do it fast (unlike Python versions).
        pkgs.whisper-cpp
        # Let's be able to view media.
        pkgs.vlc-bin
        pkgs-latest.zoom-us
      ];
      # A cloud VPN provider.  It breaks my self hosted proclivities, but
      # others can give me links to go into their VPNs.
      services.tailscale.enable = true;
      system.stateVersion = 5;
    })
  ];
}
