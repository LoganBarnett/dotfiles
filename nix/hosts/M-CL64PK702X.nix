{ host-id, flake-inputs, system, ... }: let
  username = "logan.barnett";
in {
  system.primaryUser = username;
  # Something required for every macOS host after a nix-darwin migration.  This
  # value will be different per host.  Perhaps hosts stood up after that point
  # won't need it.
  ids.gids.nixbld = 30000;
  imports = [
    ({ lib, pkgs, ... }: {
      imports = [
        ../nixos-modules/unfree-predicates.nix
      ];
      allowUnfreePackagePredicates = [
        (pkg: builtins.elem (lib.getName pkg) [
          "example-unfree-package"
          "unrar"
          "windsurf"
        ])
      ];
    })
    ../nixos-modules/secrets.nix
    flake-inputs.home-manager.darwinModules.home-manager
    # the _module.args idiom is how I can ensure these values get passed via the
    # internal callPackage mechanism for darwinSystem on these modules.  We want
    # callPackage because it does automatic "splicing" of nixpkgs to achieve
    # cross-system compiling.  I don't know that we need to use this at this
    # point, but making it all consistent has value.
    {
      _module.args.git-users = [
        {
          git-email = "logan.barnett@nwea.org";
          git-name = "Logan Barnett";
          git-signing-key = "85D2D1CE81A7A529FA4ABAE61841B0A4F704B99A";
          host-username = "logan.barnett";
        }
      ];
    }
    ../darwin.nix
    ../users/logan-new-e-ah.nix
    ../nixos-modules/user-can-admin.nix
    (import ../nixos-modules/user-can-develop.nix {
      inherit username;
    })
    ../headed-host.nix
    ({ lib, pkgs, ...}: let
      work-alias = lib.concatStrings [
        # I'm the Riddler.
        "n"
        "w"
        "e"
        "a"
      ];
    in {
      home-manager.users."logan.barnett" = {
        imports = [
          ../home-configs/gh-cli.nix
          ../home-configs/gh-cli-ache-em-ache.nix
        ];
        home.file.".gemrc".text = (pkgs.callPackage ../gemrc.nix {
          extra-gem-sources = [
            (lib.concatStrings [
              "http://gems.mgmt.${work-alias}colo.pvt:8080/"
            ])
          ];
        });
        # I used to manage .yarnrc the same way, but it has a lastUpdateCheck
        # field that is automatically written to, so it would need some
        # different plumbing.  I have examples of just writing files out that
        # can be written by external processes later, but there are better ways
        # to tackle this.
        # https://github.com/yarnpkg/yarn/issues/4134 outlines this exact issue,
        # and it is now closed because yarn v2 separates the auto-generated
        # stuff.  I don't have any projects with which to test this on yet, so
        # no action is to be taken.  I've just removed the yarnrc file instead.
        # This is the last vestiges of my yarnrc, for reference:
        # "--add.exact" true
        # email logustus@gmail.com
        # lastUpdateCheck
        # username logustus
        home.file.".npmrc".text = (pkgs.callPackage ../npmrc.nix {
          extra-npm-registries = [
            (lib.concatStrings [
              "@${work-alias}:registry="
              "https://artifacts.americas.${work-alias}.pvt/nexus/content/groups/npm-all/"
            ])
          ];
        });
      };
      environment.etc."krb5.conf".text = ''
        [libdefaults]
          default_realm = IPA.${lib.toUpper work-alias}COLO.PVT

        [realms]
          IPA.${lib.toUpper work-alias}COLO.PVT = {
            kdc = idm01.mgmt.${work-alias}colo.pvt
          }
          AMERICAS.${lib.toUpper work-alias}.PVT = {
          }
      '';
      environment.systemPackages = [
        pkgs.aider-chat
        pkgs.awscli
        # Use GitHub from the command line.
        pkgs.gh
        # Stop using the cursed GlobalProtect VPN GUI client and use something
        # we can better automate instead.
        pkgs.gpclient
        # Used for encrypting sensitive information in Hiera.
        pkgs.hiera-eyaml
        pkgs.mktemp
        # Gives us tools like ldapsearch, ldapadd, and ldapmodify which is
        # sometimes used for searching users and figuring out who the managers
        # are of employees.
        pkgs.openldap
        pkgs.openssl
        # Needed for our flavor of Hiera EYAML usage.  See `hiera-eyaml` for
        # more info.
        pkgs.saml2aws
        # Let us log in on the blasted VPN UI.  Doesn't work on macOS currently.
        # pkgs.sunshine
        # Let machines write the machine instructions.
        pkgs.windsurf
        # `jq` but for YAML.  The `yq` (no suffix) is a Python app which
        # converts YAML into JSON and then back again if desired.  So it
        # requires `jq` for operations.  This `yq` is standalone and can work
        # with YAML idioms, but isn't as mature as `jq`.
        pkgs.yq-go
      ];
      networking.hostName = host-id;
      nixpkgs.hostPlatform = system;
      nixpkgs.overlays = [
        (final: prev: let
          # Hackety hack.  See
          # https://discourse.nixos.org/t/is-it-possible-to-override-cargosha256-in-buildrustpackage/4393/20
          # but basically overrideAttrs works on mkDerivtaion but what's being
          # used is buildRustPackage which later hands things to mkDerivation.
          # Also see the in depth explanation here:
          # https://sldr.se/posts/2024/12/overriding-for-fun-and-profit/#yo-dawg-i-heard-you-like-overriding
          rustOverrideAttrs = pkg: attrs: pkg.override {
            rustPlatform = prev.rustPlatform // {
              buildRustPackage = original-args:
                prev.rustPlatform.buildRustPackage (
                  original-args // attrs
                );
            };
          };
        in {
          gpauth = prev.gpauth.overrideAttrs (old: {
            meta.platforms = old.meta.platforms ++ [ "aarch64-darwin" ];
            buildInputs = [
              pkgs.cairo
              pkgs.gtk3
              pkgs.gdk-pixbuf
              pkgs.libsoup_2_4
              pkgs.openssl
              pkgs.pango
            ];
          });
          gpclient = prev.gpclient.overrideAttrs (old: {
            buildInputs = old.buildInputs ++ [ pkgs.openssl ];
            # This may not be important.
            OPENSSL_DEV = pkgs.openssl.dev;
            # This may not be important.
            OPENSSL_INCLUDE_DIR = (
              lib.makeSearchPathOutput "dev" "include" [ pkgs.openssl.dev ]
            ) + "/openssl";
            # This is important.  Not sure why it isn't automatically sorted
            # out.
            OPENSSL_LIB_DIR = (
              lib.makeSearchPathOutput "dev" "lib" [ pkgs.openssl.dev ]
            ) + "/pkgconfig";
            # This may not be important.
            OPENSSL_STATIC = "0";
            meta.platforms = old.meta.platforms ++ [ "aarch64-darwin" ];
          });
        })
      ];
      security.pki.certificateFiles = [
        "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
        ../new-e-ah-certs.pem
        # Yabba dabba do!
        ../ach-em-ach-flintstones-ca.pem
      ];
      security.pki.keychain.certificateFiles = [
        ../new-e-ah-certs.pem
        # Yabba dabba do!
        ../ach-em-ach-flintstones-ca.pem
      ];
      # system.activationScripts.postActivation.text = ''
      # '';
      system.stateVersion = 5;
      system.activationScripts.postActivation.text = ''
        # Grant SSH access.
        dseditgroup -o edit -a logan.barnett -t user com.apple.access_ssh
        # Set the default shell.  While some systems can work fine without this
        # (I was using Terminal.app just fine while it was broken), things like
        # sshd will silently fail.
        # TODO: Make this work for multiple users.
        # TODO: Contribute this back when multiple activation scripts are
        # allowed.  This either doesn't work, or it isn't enough.  See also the
        # next command.
        echo 'Updating user shells...'
        dscl . -create /Users/logan.barnett UserShell /run/current-system/sw/bin/zsh
        # You'd think we'd just use this, but it's forcibly interactive even
        # when run as root.  Or I'm using it wrong.
        # chsh -u logan.barnett -s /run/current-system/sw/bin/zsh
        echo 'User shells updated.'
      '';
    })
    {
      imports = [
        ../nixos-modules/ollama-nix-darwin.nix
      ];
      services.ollama.enable = true;
      # services.open-webui.enable = true;
    }
  ];
}
