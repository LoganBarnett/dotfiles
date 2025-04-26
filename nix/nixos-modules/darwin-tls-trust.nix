################################################################################
#
################################################################################
{ callPackage, config, lib, pkgs, ... }: let
  cfg = config.security.pki.keychain;
in {
  options = {
    security.pki.keychain = {
      trustNixTlsCertificates = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          Enable to have the system keychain trust certificates indicated in
          security.pki.certificateFiles.
        '';
      };
    };
  };
  config = lib.mkIf cfg.trustNixTlsCertificates {
    system.activationScripts.nix-tls-trust-to-keychain.text = let
      profile = pkgs.callPackage
        ../packages/darwin-tls-certificate-configuration-profile.nix
        {
          name = "darwin-tls-certificate-configuration-profile.xml";
          # Turn into attrset because it needs names to give the certificates.
          certs = builtins.listToAttrs
            (builtins.map
              (cert-path: {
                name =
                  (builtins.baseNameOf cert-path)
                  + "-" + (builtins.hashString "sha1" (toString cert-path))
                ;
                value = cert-path;
              })
              config.security.pki.certificateFiles
            )
          ;
        };
    in ''
      sudo profiles install -type configuration -path "${profile}"
    '';
  };
}
