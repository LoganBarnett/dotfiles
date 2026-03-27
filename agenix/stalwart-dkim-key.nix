################################################################################
# Generator for an Ed25519 DKIM private key in PKCS#8 PEM form.
#
# A companion <name>.pub file containing the bare base64 public key is written
# alongside the encrypted secret and added to git automatically.  Use that
# value as the p= field in the DNS TXT record:
#   default._domainkey  TXT  "v=DKIM1; k=ed25519; p=<contents of .pub>"
################################################################################
{ pkgs, ... }:
{
  age.generators.stalwart-dkim-key =
    {
      file,
      gitAdd,
      name,
      pkgs,
      ...
    }:
    ''
      set -euo pipefail
      pub_file="$(dirname "${file}")/${name}.pub"
      ${pkgs.openssl}/bin/openssl genpkey -algorithm Ed25519 -out priv.pem
      ${pkgs.openssl}/bin/openssl pkey -in priv.pem -pubout \
        | grep -v '^-----' | tr -d '\n' > "$pub_file"
      ${gitAdd} "$pub_file"
      cat priv.pem
      rm priv.pem
    '';
}
