################################################################################
# Manage secret generators and declare all secrets here.
#
# This makes heavy use of agenix-rekey to declare secrets that are encrypted per
# host by both a master key and host keys.  These secrets do not wind up in the
# Nix store though they are manage declaratively.
#
# There are some challenges when a secret is managed or needed at _build_ time
# (as opposed to, for example, a service that reads in a secret when it starts).
# I don't have specific steps for getting around that yet.
#
# To decrypt a secret manually, use `rage --decrypt <file>.age`.  You will be
# prompted for the master password's key (which is the 3rd one I've create, "-3"
# as a suffix).  I don't know if the .pub file matters or not.
################################################################################
{
  flake-inputs,
  host-id,
}: { config, pkgs, lib, ... }: let
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
  age.generators.tls-ca-root = { file, name, pkgs, secret, ... }: let
      inherit (lib) isAttrs isString;
      inherit (lib.trivial) throwIfNot;
      inherit (secret) settings;
    in
      throwIfNot (isAttrs settings) "Secret '${name}' must have a `settings` attrset."
      validate-tls-settings name settings.tls
      '' \
      set -euo pipefail
      ${pkgs.openssl}/bin/openssl req \
         -new \
         -newkey rsa:4096 \
         -keyout root.key \
         -x509 \
         -nodes \
         -out "$(dirname "${file}")/${name}.crt" \
         -subj "/CN=${settings.tls.domain}${subject-string settings.tls.subject}" \
         -days "${toString settings.tls.validity}"
      cat root.key
      rm root.key
    '';

    # srl files can created during the leaf certificate creation.  The files
    # track which serial numbers have been used (for auditing?) so openssl
    # won't use the same one twice.  This should be checked in.
    age.generators.tls-signed-certificate = {
      decrypt,
      deps,
      file,
      name,
      pkgs,
      secret,
      ...
    }: let
      inherit (lib) isAttrs isString;
      inherit (lib.trivial) throwIfNot;
      inherit (secret) settings;
      root-cert-dep = (builtins.elemAt deps 0);
    in
      throwIfNot (isAttrs settings) "Secret '${name}' must have a `settings` attrset."
      throwIfNot (isString settings.fqdn) "Secret '${name}' is missing a `fqdn` string."
      # throwIfNot (isAttrs settings.root-certificate) "Secret '${name}' is missing a `root-certificate` value."
      ''
      set -euo pipefail
      ${decrypt} "${root-cert-dep.file}" > ca.key
      cert_path="$(dirname "${root-cert-dep.file}")/${root-cert-dep.name}.crt"
      out_file="$(dirname "${file}")/$(basename ${name} '.key').crt"
      ${pkgs.openssl}/bin/openssl req \
         -new \
         -newkey rsa:4096 \
         -sha256 \
         -nodes \
         -keyout signing.key \
         -out signing.crt \
         -subj "/CN=${settings.fqdn}${subject-string settings.root-certificate.settings.tls.subject}" \
         -addext "subjectAltName = DNS:${settings.fqdn}"
      echo "subjectAltName = DNS:${settings.fqdn}" > san.cnf
      ${pkgs.openssl}/bin/openssl x509 \
         -req \
         -in signing.crt \
         -CA $cert_path \
         -CAkey ca.key \
         -CAcreateserial \
         -out $out_file \
         -days 356 \
         -extfile san.cnf
      # Verify it works!
      ${pkgs.openssl}/bin/openssl verify \
        -CAfile $cert_path \
        $out_file \
        1>&2
      cat signing.key
      rm ca.key
      rm san.cnf
      rm signing.{crt,key}
      ''
    ;

  age.rekey = {
    # TODO:  This is the host key, and we should call it that instead of the pub
    # key.  The .pub is the pub key, but we also have a private key and having
    # that called the pub-key doesn't make sense.  Make sure to capture other
    # references, and rename what's already on disk.
    hostPubkey = ../secrets/${host-id}-pub-key.pub;
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

  age.generators.ssh-ed25519-with-pub = {
    file,
    lib,
    name,
    pkgs,
    ...
  }: ''
    mkdir -p "$(dirname "${file}")"
    (exec 3>&1;
    ${pkgs.openssh}/bin/ssh-keygen \
      -q \
      -t ed25519 \
      -N "" \
      -C ${lib.escapeShellArg "${name}"} \
      -f ${name} \
      <<<y >/dev/null 2>&1;
      cp "${name}.pub" "$(dirname "${file}")"
      echo copied public key ${name}.pub to "$(dirname "${file}")" 1>&2
      cat "${name}"
      rm "${name}"{,.pub}
    true)
  '';

  # TODO: This doesn't quite work as expected.  We need something to sync up
  # with the /etc/ssh/ssh_host_ed25519_key and its .pub sibling.  This will
  # break for new hosts trying to get secrets.  To fix it, overwrite the .pub
  # file in this repository for the host in question with
  # /etc/ssh/ssh_host_ed25519_key.pub and run `agenix rekey -a` to rekey the
  # files.
  # age.secrets."${host-id}-pub-key" = {
  #   generator.script = "ssh-ed25519-with-pub";
  #   rekeyFile = ../secrets/${host-id}-pub-key.age;
  # };

  # This is a catch22.  This is needed at build time but isn't available until
  # activation time.  See bin/nix-host-new in this repository as well as
  # bin/nix-host-key-install for putting this file in the right place.
  # environment.etc."ssh/ssh_host_ed25519_key".file = config.age.secrets."${host-id}-pub-key".file;

  age.secrets.proton-ca = {
    rekeyFile = ../secrets/proton-ca.age;
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

  age.secrets.builder-key = {
    generator.script = "ssh-ed25519-with-pub";
    rekeyFile = ../secrets/builder-key.age;
    # This used to be required due to some trouble I was having on nix-darwin
    # but no longer (see secretsDir below for information about that).
    # path = "/etc/nix/builder-agenix-key";
    path = "/etc/nix/remote-builder_ed25519";
  };

  # If you're here because you can't find /run/agenix, you're probably on
  # darwin/macOS and you don't have any host keys in /etc/ssh.  You will have to
  # generate them for this host (which can be done with `agenix-rekey
  # generate`), and then have them copied to the correct location by running
  # `nix-host-key-install` on the host in question, with this repository in
  # place (which you have to have cloned to make the executable available
  # anyways).  Then you should find a /run/agenix.  Additional notes can be
  # found in the nix-host-key-install script.
  #
  # This value is left to help me find it again when I forget all of this again.
  # I have a checklist executable started in nix-host-new.
  age.secretsDir = "/run/agenix";

  imports = [
    # This should work equally for macOS.  This and the darwinModules version
    # both reference the same file.
    flake-inputs.agenix.nixosModules.default
    flake-inputs.agenix-rekey.nixosModules.default
  ];
  environment.systemPackages = [
    # This should remain out because agenix-rekey brings in a replacement
    # agenix.
    # flake-inputs.agenix.packages.${system}.default
    pkgs.agenix-rekey
    # Rage is a Rust based Age that claims a more consistent CLI API.
    pkgs.rage
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
