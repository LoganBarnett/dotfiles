{
  flake-inputs,
  system,
  host-id,
  host-public-key ? null,
  host-public-key-file ? null
}: { pkgs, lib, ... }: let
  subject-string = subject:
    ''/C=${subject.country}\
/ST=${subject.state}\
/L=${subject.location}\
/O=${subject.organization}\
/OU=${subject.organizational-unit}''
    ;
  validate-tls-settings = let
    inherit (lib) isAttrs isInt isString;
    inherit (lib.trivial) id throwIfNot;
  in name: tls:
      throwIfNot (isAttrs tls) "Secret '${name}' must have a `tls` attrset."
      throwIfNot (isString tls.domain) "Secret '${name}' must have a `tls.domain` string."
      throwIfNot (isInt tls.validity) "Secret '${name}' must have a `tls.validity` integer."
      validate-tls-subject name tls.subject
      id
    ;

  validate-tls-subject = let
    inherit (lib) isAttrs isString;
    inherit (lib.trivial) id throwIfNot;
  in name: subject:
    throwIfNot (isAttrs subject) "Secret '${name}' must have a `tls.subject` attrset."
    throwIfNot (isString subject.country) "Secret '${name}' must have a `tls.subject.country` string."
    throwIfNot (isString subject.state) "Secret '${name}' must have a `tls.subject.state` string."
    throwIfNot (isString subject.location) "Secret '${name}' must have a `tls.subject.location` string."
    throwIfNot (isString subject.organization) "Secret '${name}' must have a `tls.subject.organization` string."
    throwIfNot (isString subject.organizational-unit) "Secret '${name}' must have a `tls.subject.organizational-unit` string."
    id
  ;
in {
  nixpkgs.overlays = [
    flake-inputs.agenix.overlays.default
    # This lets us include the agenix-rekey package.
    flake-inputs.agenix-rekey.overlays.default
  ];
  # Grants us the "long-passphrase" generator.
  # TODO: Help document pre-existing generators listed here:
  # https://github.com/oddlama/agenix-rekey/blob/85df729446fca1b9f22097b03e0ae2427c3246e2/modules/agenix-rekey.nix#L557
  age.generators.long-passphrase = {pkgs, ...}:
    "${pkgs.xkcdpass}/bin/xkcdpass --numwords=10 delimiter=' '"
  ;
  # TODO: TLS has a means of allowing certain features as well as how they
  # propagate.  Allow this to be indicated and document how this is done.  For
  # specifics on the features themselves, refer to some external documentation.
  # This applies to this generator as well as the other, related generators.
  age.generators.tls-ca-root = { pkgs, name, secret, ... }: let
      inherit (lib) isAttrs isString;
      inherit (lib.trivial) throwIfNot;
      inherit (secret) settings;
    in
      throwIfNot (isAttrs settings) "Secret '${name}' must have a `settings` attrset."
      validate-tls-settings name settings.tls
      '' \
      ${pkgs.openssl}/bin/openssl req \
         -new \
         -newkey rsa:4096 \
         -keyout root.key \
         -x509 \
         -nodes \
         -out root.crt \
         -subj "/CN=${settings.tls.domain}${subject-string settings.tls.subject}" \
         -days "${toString settings.tls.validity}"
    '';

  age.generators.tls-signing-request = { name, secret, ... }: let
      inherit (lib) isAttrs isString;
      inherit (lib.trivial) throwIfNot;
      inherit (secret) settings;
    in
      throwIfNot (isAttrs settings) "Secret '${name}' must have a `settings` attrset."
      throwIfNot (isString settings.fqdn) "Secret '${name}' must have an `fqdn` string."
      throwIfNot (isAttrs settings.root-certificate) "Secret '${name}' must have a `root-certificate` attrset."
      (validate-tls-settings name settings.tls)
      # Needs sha256 for increased security demands (citation?).
      # Needs subjectAltName for Apple devices (citation?).
      ''
      ${pkgs.openssl}/bin/openssl req \
         -new \
         -newkey rsa:4096 \
         -sha256 \
         -nodes \
         -keyout signing.key \
         -out signing.crt \
         -subj "/CN=${settings.fqdn}${subject-string settings.tls.subject}" \
         -addext "subjectAltName = DNS:${settings.fqdn}" \
      ''
    ;

  age.generators.tls-signed-certificate = { deps, secret, file, ... }: let
      inherit (lib) isAttrs isString;
      inherit (lib.trivial) throwIfNot;
      inherit (secret) settings;
    in
      throwIfNot (isAttrs settings) "Secret '${secret.name}' must have a `settings` attrset."
      throwIfNot (isString settings.fqdn) "Secret '${secret.name}' is missing a `fqdn` key."
      throwIfNot (isAttrs settings.root-certificate) "Secret '${secret.name}' is missing a `root-certificate` value."
      ''
      ${pkgs.openssl}/bin/openssl req \
         -new \
         -newkey rsa:4096 \
         -sha256 \
         -nodes \
         -keyout signing.key \
         -out signing.crt \
         -subj "/CN=${settings.fqdn}${subject-string settings.tls.subject}" \
         -addext "subjectAltName = DNS:${settings.fqdn}" \
      echo "subjectAltName = DNS:${secret.fqdn}" > san.cnf
      ${pkgs.openssl}/bin/openssl x509 \
         -req \
         -in signing.key \
         -CA ${deps[0].file}.crt \
         -CAkey ${deps[0].file}.key \
         -CAcreateserial \
         -out ${file}.crt \
         -days 356 \
         -extfile san.cnf \
      ''
    ;

  age.rekey = {
    hostPubkey = if host-public-key != null
                 then host-public-key
                 else (lib.fileContents host-public-key-file);
    masterIdentities = [
      ../secrets/agenix-master-key-3.age
    ];
    # Must be relative to the flake.nix file.
    localStorageDir = ../secrets/rekeyed/${host-id};
    generatedSecretsDir = ../secrets/generated/${host-id};
    # These fields are labeled as missing with:
    #  The option `age.rekey.userFlake' does not exist. Definition values:
    # userFlake = flake-inputs.self;
    # nodes = flake-inputs.self.nixosConfigurations;
    storageMode = "local";
  };

  age.secrets.internal-ca = {
    rekeyFile = ../secrets/internal-ca.age;
    settings = {
      tls = {
        domain = "proton";
        subject = {
          country = "US";
          state = "Oregon";
          location = "Portland";
          organization = "Barnett family";
          organizational-unit = "IT Department";
        };
        validity = 365 * 5;
      };
    };
    generator.script = "tls-ca-root";
  };

  imports = [
    flake-inputs.agenix.nixosModules.default
    flake-inputs.agenix-rekey.nixosModules.default
  ];
  environment.systemPackages = [
    # This should remain out because agenix-rekey brings in a replacement
    # agenix.
    # flake-inputs.agenix.packages.${system}.default
  ];
  # These files are not actually temporary files, especially if the "-" at the
  # end is included to keep systemd from cleaning up the directory (at some
  # point?).  See
  # https://discourse.nixos.org/t/is-it-possible-to-declare-a-directory-creation-in-the-nixos-configuration/27846/6
  # for Nix usage, and see
  # https://www.freedesktop.org/software/systemd/man/latest/tmpfiles.d.html for
  # the systemd documentation on the topic.
  # Unfortunately, there might be a loading order problem with this and other
  # build-time scripts.  Can I use a variable for the directory so it is
  # resolved before any secrets are used?  Or perhaps just some settings in
  # age.secretsDir to ensure the directory exists.
  # Since this doesn't use a declarative approach, I am opting not to use it.
  # systemd.tmpfiles.rules = [
  #   "d /etc/secrets 0770 nixbld nixbld -"
  # ];
}
