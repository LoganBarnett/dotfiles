{ flake-inputs }: let
  host-id = "M-CL64PK702X";
  system = "aarch64-darwin";
  username = "logan.barnett";
in {
  inherit system;
  modules = [
    (import ../nixos-modules/secrets.nix {
      inherit flake-inputs system;
      inherit host-id;
      host-public-key-file = ../secrets/${host-id}-ssh-key.pub;
    })
    flake-inputs.home-manager.darwinModules.home-manager
    {
      nixpkgs.overlays = [];
    }
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
    ({ pkgs, ...}: let
    in {
      environment.systemPackages = [
        pkgs.awscli
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
