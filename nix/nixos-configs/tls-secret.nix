################################################################################
# Provide TLS certificate secret generation.
################################################################################
{ lib, pkgs, ... }: let
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

  # srl files can created during the leaf certificate creation.  The files track
  # which serial numbers have been used (for auditing?) so openssl won't use the
  # same one twice.  This should be checked in.
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

}
