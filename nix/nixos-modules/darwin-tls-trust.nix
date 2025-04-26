################################################################################
#
################################################################################
{ callPackage, config, lib, pkgs, ... }: let
  cfg = config.security.pki.keychain;
        pemToCertificates = pemData: (let
          lines = lib.strings.splitString "\n" pemData;
          certs-acc = lib.lists.foldl'
          (acc: line:
            if acc.in-cert
            then (
              if line == "-----END CERTIFICATE-----"
              then {
                in-cert = false;
                cert-lines = [];
                certs = acc.certs ++ [
                  (lib.strings.concatLines acc.cert-lines)
                ];
              }
              else acc // {
                cert-lines = acc.cert-lines ++ [ line ];
              }
            )
            else (
              if line == "-----BEGIN CERTIFICATE-----"
              then acc // { in-cert = true; cert-lines = []; }
              # A comment line.  Skip it.
              else acc
            )
          )
          {
            in-cert = false;
            certs = [];
            cert-lines = [];
          }
          lines;
        in
          certs-acc.certs
        );
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
      certificateFiles = lib.mkOption {
        type = lib.types.listOf lib.types.path;
        default = [];
        description = ''
          Certificates to trust in the System Keychain.
        '';
      };
    };
  };
  config = {
    system.activationScripts.postActivation.text = let
      # sanitizeCertPath =
      santizedCertName = cert-path:
        if builtins.isPath cert-path
        then builtins.baseNameOf cert-path
        else (
          if builtins.hasContext cert-path
          then builtins.unsafeDiscardStringContext cert-path
          else builtins.baseNameOf cert-path
        )
      ;
      profile = pkgs.callPackage
        ../packages/darwin-tls-certificate-configuration-profile.nix
        {
          name = "darwin-tls-certificate-configuration-profile.xml";
          # Turn into attrset because it needs names to give the certificates.
          certs = builtins.listToAttrs
            (builtins.map
              (cert-path: let
                # Beware modifying this section - this touches a boundary of
                # Nix's store and derivation purity.  We need to support paths
                # of any kind to certificate files.  So this could be any of the
                # following:
                # - "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
                # - ./foo.pem
                # These are more different than just a String and a Path.  The
                # first entry is a String with Context and that causes some
                # difficulties with many string manipulation functions.  At the
                # moment, we use unsafeDiscardStringContext and we either
                # probably shouldn't or probably should use some other helper to
                # ensure a direct connection between the _derivation the
                # StringWithContext comes from_.
                real-cert-path =
                  # if builtins.isPath cert-path
                  # then cert-path
                  # else "asdf"
                  # if builtins.hasContext cert-path
                  # then /. + (builtins.unsafeDiscardStringContext cert-path)
                  # else cert-path
                  cert-path
                ;
                # real-cert-path = builtins.unsafeDiscardStringContext cert-path.outPath;
                cert-name = pkgs.lib.sanitizedCertName cert-path;
              in {
                name =
                  cert-name;
                #   (builtins.baseNameOf "${real-cert-path}")
                #   + "-${(builtins.hashString
                #     "sha1"
                #     (builtins.readFile real-cert-path)
                #   )}"
                # ;
                value = real-cert-path;
              })
              cfg.certificateFiles
            )
          ;
        };
      certFiles = lib.lists.flatten (builtins.map
        (path: pemToCertificates (builtins.readFile path))
        cfg.certificateFiles
      );
      # We need our own copy, because macOS' security tool only accepts a file
      # with a single certificate, and isn't strictly supporting the PEM format.
      certs = lib.lists.imap0 (i: cert: pkgs.writeTextFile (let
        # Sometimes (or always?) a linebreak winds up a the bottom of the cert,
        # and breaks formatting.  Just make sure we never have one to keep
        # things consistent.
        trimLastLinebreak = s:
          if lib.strings.hasSuffix "\n" s
          then builtins.substring 0 (builtins.stringLength s - 1) s
          else s;
      in {
        name = "nix-darwin-keychain-certificate-${toString i}.pem";
        text = ''
          -----BEGIN CERTIFICATE-----
          ${trimLastLinebreak cert}
          -----END CERTIFICATE-----
        '';
      })) certFiles;
      # Even though this is a flat string value, Nix's module system will join
      # this with other collisions of the same setting and separating them using
      # line breaks.  lib.mkBefore is just lib.mkOrder with a specific value
      # (500 I think), so we might at some point choose a different number just
      # to ensure others can also use mkBefore with a reasonable expectation
      # that they won't collide.
      # See
      # https://nixos-and-flakes.thiscute.world/nixos-with-flakes/modularize-the-configuration
      # for documentation on the topic.
      # See https://github.com/nix-darwin/nix-darwin/pull/1341 for a change to
      # nix-darwin that should allow for arbitrary activation scripts.
      command = ''
        sudo profiles install -type configuration -path "${profile}"
      '';
      oldProfileCommand = ''
        echo "Installing configuration profile to have the system keychain trust security.pki.certificateFiles..."
        echo ${command}
        ${command}
      '';
      singleSecurityAddTrustedCert = ''
        set -x
        # Note that a lot of advice out there will say to use "trustAsRoot" per
        # newer versions of macOS.  This might be correct advice in the context of
        # the question given, but since we're running _as root_ already, we can
        # just trustRoot. `trustAsRoot` is for non-root command line invocations.
        security add-trusted-cert \
          -d \
          -r trustRoot \
          -k /Library/Keychains/System.keychain \
          ${lib.strings.concatStringsSep " " certFiles}
      '';
      openssl = "${pkgs.openssl}/bin/openssl";
    in lib.mkBefore (''
      keychainTrustedCerts="$(sudo security find-certificate -a -Z \
        /Library/Keychains/System.keychain
      )"
      # Note that a lot of advice out there will say to use "trustAsRoot" per
      # newer versions of macOS.  This might be correct advice in the context of
      # the question given, but since we're running _as root_ already, we can
      # just trustRoot. `trustAsRoot` is for non-root command line invocations.
      function trustCert() {
        certFile="$1"
        certHash="$(${openssl} x509 -outform der -in "$certFile" \
          | ${openssl} dgst -sha256 \
          | awk '{print $2}'
        )"
        certDomain="$( \
          ${openssl} x509 -noout -ext subjectAltName -in "$certFile" 2>&1 \
            | grep --invert-match 'X509v3 Subject Alternative Name:' \
            | sed 's/DNS://g'
        )"
        if [[ "$certDomain" == "" \
           || "$certDomain" == 'No extensions in certificate' ]];
        then
          certDomain="$(${openssl} x509 -noout -subject -in "$certFile" \
            | sed 's/.*CN *= *//p'
          )"
         fi
        printf "Checking for trust of %s for domain '%s' found in %s... " \
          "$certHash" \
          "$certDomain" \
          "$certFile"
        if printf '%s' "$keychainTrustedCerts" \
          | grep --quiet --ignore-case "$certHash" ;
        then
          printf "\033[33mAlready Trusted.\033[0m\n"
        else
          security add-trusted-cert \
            -d \
            -r trustRoot \
            -k /Library/Keychains/System.keychain \
            "$certFile"
          printf "\033[32mTrusted!\033[0m\n"
        fi
      }
    ''
    + (lib.strings.concatLines (builtins.map (certFile: let
      in ''
        trustCert ${certFile}
      '') certs))
    );
  };
}
