{ flake-inputs }: let
  host-id = "M-CL64PK702X";
  system = "aarch64-darwin";
  username = "logan.barnett";
in {
  inherit system;
  modules = [
    (import ../nixos-modules/secrets.nix {
      inherit flake-inputs;
      inherit host-id;
    })
    flake-inputs.home-manager.darwinModules.home-manager
    # the _module.args idiom is how I can ensure these values get passed via the
    # internal callPackage mechanism for darwinSystem on these modules.  We want
    # callPackage because it does automatic "splicing" of nixpkgs to achieve
    # cross-system compiling.  I don't know that we need to use this at this
    # point, but making it all consistent has value.
    {
			_module.args.emacs-overlay = flake-inputs.emacs-overlay;
      _module.args.nixpkgs = flake-inputs.nixpkgs;
      _module.args.git-users = [
        {
          git-email = "logan.barnett@nwea.org";
          git-username = "logan-barnett-nwea";
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
        "n"
        "w"
        "e"
        "a"
      ];
    in {
      nixpkgs.overlays = [];
      home-manager.users."logan.barnett" = {
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
      environment.systemPackages = [
        pkgs.awscli
        pkgs.saml2aws
        (pkgs.callPackage ../../nix-gems/hiera-eyaml/default.nix {})
      ];
      networking.hostName = host-id;
      security.pki.certificateFiles = [
        "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
        ../new-e-ah-certs.pem
        ../secrets/proton-ca.crt
      ];
      # system.stateVersion = "23.11";
    })
  ];
}
